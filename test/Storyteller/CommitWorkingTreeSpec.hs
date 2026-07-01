{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Target contract for 'commitWorkingTree': given an arbitrary
-- before/after transformation of a file's atom chain, it must produce a
-- valid history under one rule — conservative, position-based editing.
--
-- Only *trimming* (removing some of an atom's own original bytes, from its
-- front and\/or back — never carving out its middle) can change an atom's
-- classification. Padding alone, with no trim, is indistinguishable from an
-- adjacent insertion and so is never attributed to an untouched atom —
-- that's what makes the rule unambiguous instead of a generic (and
-- inherently ambiguous) text-diff problem:
--
--   * Untouched on both sides (no trim) → *kept*: same tick id, no entry
--     in the returned mapping, unconditionally — nothing ever folds onto
--     an atom that wasn't otherwise going to change.
--   * Trimmed on either side, with nonzero content remaining once any
--     immediately-adjacent new bytes are folded in → *changed*: new tick
--     at the same position, and (since the tick id necessarily changes) a
--     mapping entry recording old→new so references get updated.
--   * Trimmed down to nothing (after folding) → *dropped*: the tick
--     disappears from the chain entirely, like 'Storyteller.Edit.deleteTick'.
--   * New content between two atoms folds onto the preceding atom's back
--     if it's already changing there, else the following atom's front if
--     it's already changing there (fewer created ticks either way), else
--     it's a standalone new tick unrelated to any existing atom.
--
-- These properties currently FAIL: 'commitWorkingTree' only supports pure
-- growth today (see 'Storyteller.Edit.ModificationDetected' — any file
-- whose total length shrinks is rejected outright, and the position-based
-- walk it does use assumes monotonic growth, so even same-total-length
-- edits are silently misattributed rather than rejected). This spec is the
-- target behavior to implement against, not a description of current code.
module Storyteller.CommitWorkingTreeSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Test.Hspec
import Test.QuickCheck

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState, State)

import Git.Mock
import Runix.Git (Git)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, readFile, writeFile, appendFile)
import qualified Data.List as List
import Prelude hiding (readFile, writeFile, appendFile, drop)

import Storyteller.Atom (Atom(..))
import Storyteller.Git
import Storyteller.Storage hiding (get, drop)
import qualified Storyteller.Storage as S
import Storyteller.Types
import Storyteller.Edit (commitWorkingTree)

-- ---------------------------------------------------------------------------
-- Phantom branch tag + fixed path (single-file focus; multi-file is a
-- separate, later concern — this vocabulary generalizes to it directly by
-- generating a 'Transform' per path).
-- ---------------------------------------------------------------------------

data Main

path :: FilePath
path = "f.md"

-- ---------------------------------------------------------------------------
-- Runner
-- ---------------------------------------------------------------------------

runCWT
  :: Sem '[ StoryBranch Main
          , FileSystemWrite (BranchTag Main)
          , FileSystemRead  (BranchTag Main)
          , FileSystem      (BranchTag Main)
          , StoryStorage
          , Git
          , State WorkingTree
          , State GitState
          , Fail
          ] a
  -> Either String a
runCWT action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . evalState emptyWorkingTree
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "main")
      runStoryFSGit @Main (BranchName "main")
        . runStoryBranchGit @Main (BranchName "main")
        . subsume_
        $ action

-- | Store a fresh sequence of atoms into 'path', one tick each, oldest
--   first — mirrors how real appends build up a file's history.
storeAtoms
  :: Members '[ StoryBranch Main, FileSystem (BranchTag Main)
              , FileSystemRead (BranchTag Main), FileSystemWrite (BranchTag Main)
              , StoryStorage, Fail ] r
  => [BS.ByteString] -> Sem r [TickId]
storeAtoms = mapM storeOne
  where
    storeOne content = do
      appendFile @(BranchTag Main) path content
      storeAs @Main (Atom path "")

-- | All tick ids currently reachable on the chain, oldest-first, root
--   excluded — used to check that dropped atoms are truly gone and kept
--   atoms truly remain.
chainIds
  :: Members '[StoryBranch Main, Fail] r
  => Sem r [TickId]
chainIds = do
  ticks <- S.follow @Main [] (\acc t -> (t : acc, tickParent t))
  return [ tickId t | t <- ticks, tickParent t /= Nothing ]

-- ---------------------------------------------------------------------------
-- Vocabulary: an atom chain and an arbitrary transformation of it
-- ---------------------------------------------------------------------------

-- | Possibly-empty uppercase chunk — new (inserted) content. Uppercase
--   keeps it disjoint from atom content by construction, so there is never
--   a question of whether a byte "belongs" to an atom's own text or to
--   something newly typed next to it.
newtype Gap = Gap { unGap :: BS.ByteString } deriving (Show, Eq)

instance Arbitrary Gap where
  arbitrary = Gap . BSC.pack <$> frequency
    [ (2, pure [])
    , (1, choose (3, 8) >>= (`vectorOf` choose ('A', 'Z')))
    ]
  shrink (Gap bs) = [ Gap (BSC.pack s) | s <- shrink (BSC.unpack bs) ]

-- | How a single existing atom's content is transformed: trim bytes off
--   the front and\/or back. Trimming is the only thing that can force a
--   change — see the module header.
data AtomEdit = AtomEdit
  { editTrimFront :: Int
  , editTrimBack  :: Int
  } deriving (Show, Eq)

noEdit :: AtomEdit
noEdit = AtomEdit 0 0

trimAtom :: BS.ByteString -> AtomEdit -> BS.ByteString
trimAtom orig (AtomEdit tf tb) =
  let n   = BS.length orig
      tf' = max 0 (min tf n)
      tb' = max 0 (min tb (n - tf'))
  in BS.take (n - tf' - tb') (BS.drop tf' orig)

data AtomOutcome = Kept | Changed | Dropped deriving (Show, Eq)

-- | A full transformation of an N-atom chain: one edit per atom, plus N+1
--   gaps of newly-inserted content — gap @i@ sits before atom @i@ (0-based);
--   the final gap (index N) sits after the last atom.
data Transform = Transform
  { tAtoms :: [BS.ByteString]
  , tEdits :: [AtomEdit]
  , tGaps  :: [BS.ByteString]
  } deriving (Show)

buildAfter :: Transform -> BS.ByteString
buildAfter (Transform atoms edits gaps) =
  BS.concat (interleave gaps (zipWith trimAtom atoms edits))
  where
    interleave (g : gs) (e : es) = g : e : interleave gs es
    interleave [g] []            = [g]
    interleave gs' _             = gs'

-- | Whether an atom is eligible to absorb adjacent new content: it must be
--   *partially* trimmed — a nonempty core remains, but it's not the whole
--   original. A fully-kept atom (the whole original survives) was never
--   going to change, so nothing folds onto it. A fully-dropped atom (empty
--   core) leaves no surviving anchor at all — there's no principled way to
--   say adjacent new content "belongs" to something entirely gone. Only a
--   partial trim, with some recognizable remainder, is eligible.
eligible :: Transform -> [Bool]
eligible (Transform atoms edits _) =
  zipWith (\a e -> let core = trimAtom a e in not (BS.null core) && BS.length core < BS.length a) atoms edits

data GapFate = FoldBack | FoldFront | Standalone deriving (Show, Eq)

-- | Attribute each gap to a neighbor (folded) or mark it standalone: fold
--   onto the preceding atom's back if it's eligible, else the following
--   atom's front if it's eligible, else standalone. (Both being eligible
--   is a tie broken toward the preceding atom — arbitrary but deterministic;
--   either choice costs the same number of created ticks.)
gapFates :: Transform -> [GapFate]
gapFates t@(Transform atoms _ gaps) = [ fate i | i <- [0 .. length gaps - 1] ]
  where
    n    = length atoms
    elig = eligible t
    fate i
      | BS.null (gaps !! i)       = Standalone
      | i > 0 && elig !! (i - 1)  = FoldBack
      | i < n && elig !! i        = FoldFront
      | otherwise                 = Standalone

-- | Each atom's final content: its trimmed core plus whatever gaps folded
--   onto its front\/back.
finalAtomContents :: Transform -> [BS.ByteString]
finalAtomContents t@(Transform atoms edits gaps) =
  [ frontFold i <> trimAtom (atoms !! i) (edits !! i) <> backFold i | i <- [0 .. n - 1] ]
  where
    n     = length atoms
    fates = gapFates t
    frontFold i = if fates !! i == FoldFront then gaps !! i else BS.empty
    backFold i  = if fates !! (i + 1) == FoldBack then gaps !! (i + 1) else BS.empty

outcomes :: Transform -> [AtomOutcome]
outcomes t@(Transform _ edits _) =
  [ if e == noEdit then Kept else if BS.null fc then Dropped else Changed
  | (e, fc) <- zip edits (finalAtomContents t) ]

instance Arbitrary Transform where
  arbitrary = do
    n     <- choose (1, 5)
    atoms <- mapM genAtom [0 .. n - 1]
    edits <- mapM (genEdit . BS.length) atoms
    gaps  <- map unGap <$> vectorOf (n + 1) arbitrary
    return $ Transform atoms edits gaps
    where
      -- Each atom draws from its own disjoint 5-letter window of the
      -- alphabet (atom 0 from 'a'-'e', atom 1 from 'f'-'j', ...). No
      -- substring of one atom — trimmed or not — can ever coincide with
      -- another atom's, which rules out accidental cross-atom matches
      -- confusing the longest-common-substring recovery. That's a
      -- different concern from the atom/gap alphabet split above: this
      -- one is about atoms not being mistaken for *each other*.
      genAtom i = do
        let base = fromEnum 'a' + i * 5
            lo   = toEnum base
            hi   = toEnum (base + 4)
        len <- choose (3, 5)
        BSC.pack <$> vectorOf len (choose (lo, hi))
      genEdit len = do
        useEdit <- arbitrary
        if not useEdit then return noEdit else do
          tf <- choose (0, len)
          tb <- choose (0, len - tf)
          return $ AtomEdit tf tb

  shrink (Transform atoms edits gaps) =
    dropLastAtom ++ toNoEdits ++ toEmptyGaps
    where
      n = length atoms
      dropLastAtom
        | n <= 1    = []
        | otherwise =
            [ Transform (init atoms) (init edits)
                        (init (init gaps) ++ [last (init gaps) <> last gaps]) ]
      toNoEdits =
        [ Transform atoms (replaceAt i noEdit edits) gaps
        | i <- [0 .. n - 1], edits !! i /= noEdit ]
      toEmptyGaps =
        [ Transform atoms edits (replaceAt i BS.empty gaps)
        | i <- [0 .. length gaps - 1], not (BS.null (gaps !! i)) ]
      replaceAt i x xs = take i xs ++ [x] ++ List.drop (i + 1) xs

-- ---------------------------------------------------------------------------
-- Executing a Transform through the real effect stack
-- ---------------------------------------------------------------------------

data CWTResult = CWTResult
  { rOldIds   :: [TickId]
  , rMapping  :: [(TickId, TickId)]
  , rContent  :: BS.ByteString
  , rChainIds :: [TickId]
  }

execTransform :: Transform -> Either String CWTResult
execTransform t = runCWT $ do
  oldIds  <- storeAtoms (tAtoms t)
  writeFile @(BranchTag Main) path (buildAfter t)
  mapping <- commitWorkingTree @(BranchTag Main) @Main
  -- Reload from HEAD rather than trusting the ambient buffer we just wrote —
  -- this is the only way to see what actually landed in the chain.
  S.reset @Main
  content <- readFile @(BranchTag Main) path
  ids     <- chainIds
  return $ CWTResult oldIds mapping content ids

checkResult :: Transform -> CWTResult -> Expectation
checkResult t (CWTResult oldIds mapping content chain) = do
  content `shouldBe` buildAfter t
  mapM_ checkOutcome (zip oldIds (outcomes t))
  where
    checkOutcome (oid, Kept) =
      -- A kept atom's own content and position never change, but its tick id
      -- can still be reassigned if a later 'at' rebases the tail past it —
      -- tick ids are hashes of (content, parent), so any tail rebase mints a
      -- new id for everything downstream regardless of content (see
      -- DATA-MODEL.md, "Tick ID Stability"). What must hold is that the atom's
      -- identity survives — directly, or via the mapping if it was swept in.
      chain `shouldSatisfy` elem (maybe oid id (lookup oid mapping))
    checkOutcome (oid, Dropped) =
      chain `shouldSatisfy` notElem oid
    checkOutcome (oid, Changed) =
      lookup oid mapping `shouldSatisfy` \case
        Just _  -> True
        Nothing -> False

checkTransform :: Transform -> Expectation
checkTransform t = case execTransform t of
  Left err     -> expectationFailure err
  Right result -> checkResult t result

prop_commitWorkingTree :: Transform -> Property
prop_commitWorkingTree t = case execTransform t of
  Left err     -> counterexample ("commitWorkingTree failed: " <> err) False
  Right result -> conjoin
    [ counterexample "roundtrip content mismatch"
        (rContent result === buildAfter t)
    , conjoin
        [ counterexample (show oid <> " (Kept) identity must survive, directly or via the mapping")
            (maybe oid id (lookup oid (rMapping result)) `elem` rChainIds result)
        | (oid, Kept) <- zip (rOldIds result) (outcomes t) ]
    , conjoin
        [ counterexample (show oid <> " (Dropped) must not be in the chain") (oid `notElem` rChainIds result)
        | (oid, Dropped) <- zip (rOldIds result) (outcomes t) ]
    , conjoin
        [ counterexample (show oid <> " (Changed) must have a reference-update mapping entry")
            (case lookup oid (rMapping result) of { Just _ -> True; Nothing -> False })
        | (oid, Changed) <- zip (rOldIds result) (outcomes t) ]
    ]

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "commitWorkingTree QuickCheck" $
    it "any before/after transformation of an atom chain produces a valid, conservative history" $
      property prop_commitWorkingTree

  describe "commitWorkingTree known side cases" $ do

    it "edit at the front of a single atom (trim front, new prefix folds in)" $
      checkTransform $ Transform ["hello world"] [AtomEdit 6 0] ["goodbye ", ""]

    it "edit at the back of a single atom (trim back, new suffix folds in)" $
      checkTransform $ Transform ["hello world"] [AtomEdit 0 5] ["", "!!!"]

    it "edit of a middle atom, trimmed on both sides, neighbors untouched" $
      checkTransform $ Transform ["aaaaaa", "middle content here", "bbbbbb"]
                                  [noEdit, AtomEdit 3 4, noEdit]
                                  ["", "", "", ""]

    it "deletion shorter than an atom's length stays nonzero: a change, not a drop" $
      checkTransform $ Transform ["hello world"] [AtomEdit 0 6] ["", ""]

    it "deletion exactly consuming a middle atom drops just that one" $
      checkTransform $ Transform ["aaaaaa", "bbbbbb", "cccccc"]
                                  [noEdit, AtomEdit 6 0, noEdit]
                                  ["", "", "", ""]

    it "deletion spanning multiple whole atoms drops all of them" $
      checkTransform $ Transform ["aaaaaa", "bbbbbb", "cccccc", "dddddd"]
                                  [noEdit, AtomEdit 6 0, AtomEdit 0 6, noEdit]
                                  ["", "", "", "", ""]

    it "collapsing the whole chain drops every atom" $
      checkTransform $ Transform ["aaaaaa", "bbbbbb"]
                                  [AtomEdit 6 0, AtomEdit 6 0]
                                  ["", "", ""]

    it "pure addition between two untouched atoms is a new tick, not a change to either" $
      checkTransform $ Transform ["aaaaaa", "bbbbbb"] [noEdit, noEdit] ["", "NEW", ""]

    it "pure addition before the first atom" $
      checkTransform $ Transform ["aaaaaa"] [noEdit] ["FRONT", ""]

    it "pure addition after the last atom" $
      checkTransform $ Transform ["aaaaaa"] [noEdit] ["", "END"]

    it "a gap between two independently-trimmed atoms folds onto the preceding one (tie-break)" $
      checkTransform $ Transform ["aaaaaa", "bbbbbb"]
                                  [AtomEdit 0 3, AtomEdit 3 0]
                                  ["", "MID", ""]
