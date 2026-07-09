{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Target contract for 'commitWorktree' (and 'commitFile'): given an
--   arbitrary before/after transformation of a file's atom chain, it must
--   fold the difference into the chain using only 'store'\/'drop'\/'at' --
--   see "Storage.Ops"'s own Haddock for the exact classification rule
--   (Kept\/Changed\/Dropped\/standalone). This ports the same contract the
--   old 'Storyteller.Core.StorageMonad.commitWorkingTree' was checked
--   against (see 'Storyteller.CommitWorkingTreeSpec'), but only through
--   what "Storage.Ops" actually exports: no id\/mapping is returned at
--   this layer (see the module Haddock's remark that cross-chain
--   reference updates are an app-level concern above this one), so
--   correctness is checked structurally -- final content, and each
--   surviving atom's own content and position in the chain -- rather
--   than by id.
module Storage.CommitWorktreeSpec (spec) where

import Prelude hiding (drop, readFile, writeFile)

import Control.Monad.State.Strict (lift)
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Test.Hspec
import Test.QuickCheck

import Storage.Core
import Storage.FS (remove, list)
import Storage.Ops
import Storage.MockStore

path :: FilePath
path = "f.md"

-- ---------------------------------------------------------------------------
-- Reading back the chain -- test-side only, built from exported
-- "Storage.Core" primitives, the same way "Storage.Ops" itself is.
-- ---------------------------------------------------------------------------

-- | Every atom currently on @path@'s own history, oldest first -- content
--   only, since 'Storage.Ops' hands back no id\/mapping at this layer.
atomContents :: StoreM m => FilePath -> StoreT m [Text]
atomContents p = headHash >>= \h -> go h []
  where
    go h acc = do
      t  <- lift (readTick h)
      cd <- lift (readCommit h)
      let acc' = case t of
            Atom _ p' _ content | p' == p -> content : acc
            _                           -> acc
      case commitParents cd of
        []      -> return acc'
        (par:_) -> go par acc'

-- | Every non-root tick id reachable from head -- used to check that a
--   fully-deleted file's atoms are truly gone from the chain, not just
--   absent from the ambient tree.
chainIds :: StoreM m => StoreT m [ObjectHash]
chainIds = headHash >>= go []
  where
    go acc h = do
      cd <- lift (readCommit h)
      case commitParents cd of
        []      -> return acc
        (par:_) -> go (h : acc) par

-- ---------------------------------------------------------------------------
-- Vocabulary: an atom chain and an arbitrary transformation of it -- same
-- shape as 'Storyteller.CommitWorkingTreeSpec', ported to 'Text'.
-- ---------------------------------------------------------------------------

newtype Gap = Gap { unGap :: Text } deriving (Show, Eq)

instance Arbitrary Gap where
  arbitrary = Gap . T.pack <$> frequency
    [ (2, pure [])
    , (1, choose (3, 8) >>= (`vectorOf` choose ('A', 'Z')))
    ]
  shrink (Gap t) = [ Gap (T.pack s) | s <- shrink (T.unpack t) ]

data AtomEdit = AtomEdit { editTrimFront :: Int, editTrimBack :: Int } deriving (Show, Eq)

noEdit :: AtomEdit
noEdit = AtomEdit 0 0

trimAtom :: Text -> AtomEdit -> Text
trimAtom orig (AtomEdit tf tb) =
  let n   = T.length orig
      tf' = max 0 (min tf n)
      tb' = max 0 (min tb (n - tf'))
  in T.take (n - tf' - tb') (T.drop tf' orig)

data AtomOutcome = Kept | Changed | Dropped deriving (Show, Eq)

-- | One edit per atom, plus N+1 gaps of newly-inserted content -- gap @i@
--   sits before atom @i@ (0-based); the final gap sits after the last atom.
data Transform = Transform
  { tAtoms :: [Text]
  , tEdits :: [AtomEdit]
  , tGaps  :: [Text]
  } deriving Show

buildAfter :: Transform -> Text
buildAfter (Transform atoms edits gaps) =
  T.concat (interleave gaps (zipWith trimAtom atoms edits))
  where
    interleave (g:gs) (e:es) = g : e : interleave gs es
    interleave [g] []        = [g]
    interleave gs' _         = gs'

eligible :: Transform -> [Bool]
eligible (Transform atoms edits _) =
  zipWith (\a e -> let core = trimAtom a e in not (T.null core) && T.length core < T.length a) atoms edits

data GapFate = FoldBack | FoldFront | Standalone deriving (Show, Eq)

gapFates :: Transform -> [GapFate]
gapFates t@(Transform atoms _ _) = gapFates' atoms (eligible t) (mergedGaps t)

gapFates' :: [Text] -> [Bool] -> [Text] -> [GapFate]
gapFates' atoms elig gaps = [ fate i | i <- [0 .. length gaps - 1] ]
  where
    n = length atoms
    fate i
      | T.null (gaps !! i)       = Standalone
      | i > 0 && elig !! (i - 1) = FoldBack
      | i < n && elig !! i       = FoldFront
      | otherwise                = Standalone

finalAtomContents :: Transform -> [Text]
finalAtomContents t@(Transform atoms edits _) = finalAtomContents' cores (mergedGaps t) (gapFates t)
  where cores = zipWith trimAtom atoms edits

finalAtomContents' :: [Text] -> [Text] -> [GapFate] -> [Text]
finalAtomContents' cores gaps fates =
  [ frontFold i <> (cores !! i) <> backFold i | i <- [0 .. n - 1] ]
  where
    n = length cores
    frontFold i = if fates !! i == FoldFront then gaps !! i else T.empty
    backFold i  = if fates !! (i + 1) == FoldBack then gaps !! (i + 1) else T.empty

-- | Kept\/Changed\/Dropped is fully determined by an atom's own trim,
--   independent of any gap folding: folding only ever adds content to an
--   atom that already has a nonempty surviving core (see 'eligible'), so
--   it can never turn a fully-trimmed (Dropped) atom into a nonempty one.
outcomes :: Transform -> [AtomOutcome]
outcomes (Transform atoms edits _) = zipWith outcomeOf atoms edits
  where
    outcomeOf a e
      | e == noEdit           = Kept
      | T.null (trimAtom a e) = Dropped
      | otherwise             = Changed

-- | The gaps actually seen by matching, as opposed to what the 'Transform'
--   nominally specifies: a to-be-dropped atom's own trimmed core is empty,
--   so its match in the flattened target sits with zero length exactly at
--   the running cursor (see 'Storage.Ops.longestCommonSubstring' -- the
--   no-overlap case always returns offset 0), meaning *none* of the
--   target text between the previous match and this one gets attributed
--   as "this atom's own leading gap" -- gap slot @i@ (the naive gap
--   immediately before atom @i@) comes back empty, and everything that
--   would've been there carries forward, landing on whichever gap slot
--   comes after the next atom that actually keeps some content (or the
--   trailing slot, if none do). Same length as the naive gaps (n+1,
--   aligned to the same positions) -- only the contents shift.
mergedGaps :: Transform -> [Text]
mergedGaps t@(Transform _ _ gaps) = go (outcomes t) gaps T.empty
  where
    go (o : os) (g : gs) carry
      | o == Dropped = T.empty : go os gs (carry <> g)
      | otherwise    = (carry <> g) : go os gs T.empty
    go [] [gLast] carry = [carry <> gLast]
    go _  _       _     = [] -- unreachable: gaps is always one longer than atoms

-- | The full ordered content of every atom the chain should hold after
--   reconciliation. Mirrors 'Storage.Ops.reconcileFile'\/'emitStandaloneGap'
--   exactly -- fold eligibility and standalone-gap placement are computed
--   over the *full*, original atom positions (a dropped atom is still a
--   real, ineligible entry that blocks folding across it, not simply
--   removed -- see 'mergedGaps' for the one thing that does shift), with
--   only the dropped atoms' own (always-empty) content omitted from the
--   final list. A target that reconciles down to nothing still leaves one
--   empty marker atom behind (see 'Storage.Ops.commitFile') -- the same
--   convention a brand new empty file gets.
predictedAtoms :: Transform -> [Text]
predictedAtoms t@(Transform atoms _ _) =
  let gaps  = mergedGaps t
      fates = gapFates t
      fcs   = finalAtomContents t
      os    = outcomes t
      n     = length atoms
      standalone i = [ gaps !! i | fates !! i == Standalone, not (T.null (gaps !! i)) ]
      raw   = concat [ standalone i ++ [ fcs !! i | os !! i /= Dropped ] | i <- [0 .. n - 1] ] ++ standalone n
  in if null raw then [""] else raw

instance Arbitrary Transform where
  arbitrary = do
    n     <- choose (1, 5)
    atoms <- mapM genAtom [0 .. n - 1]
    edits <- mapM (genEdit . T.length) atoms
    gaps  <- map unGap <$> vectorOf (n + 1) arbitrary
    return $ Transform atoms edits gaps
    where
      -- Each atom draws from its own disjoint 5-letter window of the
      -- alphabet so no substring of one atom -- trimmed or not -- can ever
      -- coincide with another's, ruling out accidental cross-atom matches
      -- confusing the longest-common-substring recovery.
      genAtom i = do
        let base = fromEnum 'a' + i * 5
            lo   = toEnum base
            hi   = toEnum (base + 4)
        len <- choose (3, 5)
        T.pack <$> vectorOf len (choose (lo, hi))
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
        [ Transform atoms edits (replaceAt i T.empty gaps)
        | i <- [0 .. length gaps - 1], not (T.null (gaps !! i)) ]
      replaceAt i x xs = take i xs ++ [x] ++ List.drop (i + 1) xs

-- ---------------------------------------------------------------------------
-- Executing a Transform through the real (mock-backed) effect stack
-- ---------------------------------------------------------------------------

data CWTResult = CWTResult
  { rContent      :: Text
  , rAtomContents :: [Text]
  }

execTransform :: Transform -> Either String CWTResult
execTransform t = fst <$> runChain (do
  _ <- mapM (addAtom path) (tAtoms t)
  writeFile path (TE.encodeUtf8 (buildAfter t))
  commitWorktree
  content <- committedContent path
  atoms   <- atomContents path
  return $ CWTResult (TE.decodeUtf8 content) atoms)

prop_commitWorktree :: Transform -> Property
prop_commitWorktree t = case execTransform t of
  Left err     -> counterexample ("commitWorktree failed: " <> err) False
  Right result -> conjoin
    [ counterexample "roundtrip content mismatch" (rContent result === buildAfter t)
    , counterexample "surviving atom contents/positions mismatch"
        (rAtomContents result === predictedAtoms t)
    ]

checkTransform :: Transform -> Expectation
checkTransform t = case execTransform t of
  Left err     -> expectationFailure err
  Right result -> do
    rContent result `shouldBe` buildAfter t
    rAtomContents result `shouldBe` predictedAtoms t

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "commitWorktree QuickCheck" $
    it "any before/after transformation of an atom chain folds into a valid, conservative history" $
      property prop_commitWorktree

  describe "commitWorktree known side cases" $ do

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

  describe "commitWorktree: files with no prior history, and multiple files" $ do

    it "a brand new file with no atom history is introduced whole" $ do
      let result = fst <$> runChain (do
            writeFile "new.md" (TE.encodeUtf8 "hello\n")
            commitWorktree
            committedContent "new.md")
      result `shouldBe` Right "hello\n"

    it "reconciles each ambient file against its own history independently" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "a.md" "aaa"
            _ <- addAtom "b.md" "bbb"
            writeFile "a.md" (TE.encodeUtf8 "aaaXXX")
            writeFile "b.md" (TE.encodeUtf8 "YYYbbb")
            commitWorktree
            ca <- committedContent "a.md"
            cb <- committedContent "b.md"
            return (ca, cb))
      result `shouldBe` Right ("aaaXXX", "YYYbbb")

    it "a file removed entirely from the ambient tree drops its whole committed history" $ do
      let result = runChain (do
            _ <- addAtom "gone.md" "some content"
            remove "gone.md"
            commitWorktree
            stillPresent <- elem "gone.md" <$> inWorktree list
            ids          <- chainIds
            return (stillPresent, ids))
      case result of
        Left err -> expectationFailure err
        Right ((stillPresent, _ids), _finalState) -> stillPresent `shouldBe` False

    -- Reproduces a whole-file-delete bug: a file introduced via
    -- 'Storyteller.Core.Create.createFile' has exactly one atom, with
    -- empty content (the path's own introduction — see its Haddock).
    -- 'longestCommonSubstring' short-circuits to (0,0,0) whenever either
    -- input is empty, so a zero-length original atom always has
    -- 'amCoreLen == amOriginalLen == 0' regardless of the target — 'isKept'
    -- read that as "fully recovered, unchanged" unconditionally, so this
    -- one atom could never be dropped no matter what the target said,
    -- and a file that was created but never appended to could never be
    -- deleted at all.
    it "a file whose only atom has empty content is still fully dropped when removed" $ do
      let result = runChain (do
            _ <- addAtom "gone.md" ""
            remove "gone.md"
            commitWorktree
            stillPresent <- elem "gone.md" <$> inWorktree list
            ids          <- chainIds
            return (stillPresent, ids))
      case result of
        Left err -> expectationFailure err
        Right ((stillPresent, _ids), _finalState) -> stillPresent `shouldBe` False

    -- The exact shape 'Storyteller.Core.Create.deleteFile' reconciles: a
    -- 'createFile'-style empty introduction atom followed by real appended
    -- content, all removed via 'commitFiles' (the scoped reconciler
    -- 'deleteFile' actually calls, not the whole-tree 'commitWorktree').
    -- Reproduces the original bug report exactly: content vanished from the
    -- tree, but the file's empty "creation" atom stuck around in the chain
    -- -- 'atomHistory' (unlike a plain tree-presence check) still walks
    -- past it regardless of what the final tree looks like, so a stray
    -- "kept" atom here would leave the path permanently un-recreatable
    -- ('Server.Core.File.createFile's own "already exists" guard reads
    -- exactly this). Fixed at the root in 'isKept' -- see its own Haddock
    -- -- so plain 'commitFile' now drops it too, not just 'commitFiles'.
    it "commitFiles drops a created-then-appended file's empty creation atom too, not just its content" $ do
      let result = runChain (do
            _ <- addAtom "gone.md" ""
            _ <- addAtom "gone.md" "real content"
            remove "gone.md"
            commitFiles ["gone.md"]
            stillPresent <- elem "gone.md" <$> inWorktree list
            history      <- atomHistory "gone.md"
            return (stillPresent, history))
      case result of
        Left err -> expectationFailure err
        Right ((stillPresent, history), _finalState) -> do
          stillPresent `shouldBe` False
          history `shouldBe` []

    it "commitFile alone (the bare, unscoped reconciler) also fully drops an emptied file's creation atom" $ do
      let result = runChain (do
            _ <- addAtom "gone.md" ""
            _ <- addAtom "gone.md" "real content"
            remove "gone.md"
            commitFile "gone.md"
            atomHistory "gone.md")
      case result of
        Left err -> expectationFailure err
        Right (history, _finalState) -> history `shouldBe` []

  -- A deletion marker ('Storage.Ops.deleteFile') is a permanent, forward
  -- record: 'Storage.Core.store's own Haddock promises every atom from
  -- before the deletion stays on the chain (a 'readAt' to any tick before
  -- it still sees the file exactly as it was), and 'atomHistory' treats
  -- the marker as the boundary of the path's *current* lifetime.
  -- Reconciliation, though, receives that marker as just another
  -- zero-length atom -- and a zero-length atom can never classify as Kept
  -- (see 'isKept'), so the first reconcile pass over a recreated path
  -- Drops the marker, un-hiding the old lifetime mid-loop while the loop
  -- keeps addressing atoms by index into a freshly re-walked (now longer)
  -- 'atomHistory'. Depending on the old life's shape, the old content is
  -- silently rebased out of the chain, or the leftover tree mismatch trips
  -- 'syncOpaqueContent's tracked-path guard and every commitWorktree on
  -- the branch fails from then on.
  describe "commitWorktree: delete-then-recreate lifetimes" $ do

    -- Oldest-first (tags, content) of every atom on @p@ anywhere in the
    -- chain -- deliberately *not* 'atomHistory', which stops at the most
    -- recent deletion marker: these tests are exactly about what survives
    -- beyond that boundary.
    let allAtomsOn p = follow [] $ \acc _ t -> case t of
          Atom _ p' tags c | p' == p -> ((tags, c) : acc, True)
          _                          -> (acc, True)

    it "an untouched recreated file keeps its old life and deletion marker across commitWorktree" $ do
      let result = runChain (do
            _ <- addAtom "scene.md" "old"
            _ <- deleteFile "scene.md"
            _ <- addAtom "scene.md" ""      -- recreation, createFile-shaped
            _ <- addAtom "scene.md" "new"
            commitWorktree
            content <- committedContent "scene.md"
            atoms   <- allAtomsOn "scene.md"
            return (content, atoms))
      case result of
        Left err -> expectationFailure err
        Right ((content, atoms), _finalState) -> do
          content `shouldBe` "new"
          map snd atoms `shouldContain` ["old"]
          filter (isRemoval . fst) atoms `shouldSatisfy` (not . null)

    it "commitWorktree still succeeds when the deleted life had more than one atom" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "scene.md" "old1"
            _ <- addAtom "scene.md" "old2"
            _ <- deleteFile "scene.md"
            _ <- addAtom "scene.md" ""
            _ <- addAtom "scene.md" "new"
            commitWorktree
            committedContent "scene.md")
      result `shouldBe` Right "new"

  -- Non-UTF8 ambient content — see the "one small can of worms" design
  -- conversation: a path that was *never* atom-tracked (dropped in by
  -- hand, e.g. via the git CLI, or part of a whole pre-existing repo
  -- adopted wholesale as a StoryStorage backing branch) must not crash
  -- commitWorktree at all — 'commitFile' leaves it alone, and
  -- 'syncOpaqueContent' folds it (and anything else untracked) into one
  -- opaque commit so it actually persists, without needing to know or
  -- enumerate which specific paths it covers. A path that's *already*
  -- atom-tracked has no such escape hatch: its own fold-of-atoms
  -- invariant is load-bearing, so external tampering into non-UTF8
  -- content must still surface as a loud failure, not a silent skip.
  describe "commitWorktree: non-UTF8 ambient content" $ do
    let binaryBytes = BS.pack [0xFF, 0xFE, 0x00, 0x01, 0x02]

    it "a never-atom-tracked binary file survives commitWorktree untouched, with no atom history" $ do
      let result = runChain (do
            writeFile "portrait.png" binaryBytes
            commitWorktree
            stillPresent <- elem "portrait.png" <$> inWorktree list
            history      <- atomHistory "portrait.png"
            content      <- committedContent "portrait.png"
            return (stillPresent, history, content))
      case result of
        Left err -> expectationFailure err
        Right ((stillPresent, history, content), _finalState) -> do
          stillPresent `shouldBe` True
          history `shouldBe` []
          content `shouldBe` binaryBytes

    it "a never-atom-tracked binary file doesn't disturb a sibling text file's own reconciliation" $ do
      let result = fst <$> runChain (do
            _ <- addAtom "scene.md" "p1\n"
            writeFile "portrait.png" binaryBytes
            writeFile "scene.md" (TE.encodeUtf8 "p1XXX")
            commitWorktree
            committedContent "scene.md")
      result `shouldBe` Right (TE.encodeUtf8 "p1XXX")

    it "an already atom-tracked file clobbered with binary content still crashes commitWorktree" $ do
      let result = runChain (do
            _ <- addAtom "scene.md" "p1\n"
            writeFile "scene.md" binaryBytes
            commitWorktree)
      case result of
        Left _  -> return () -- expected: reconcileFile's own readWorking fails loudly
        Right _ -> expectationFailure "expected commitWorktree to fail on non-UTF8 content over an atom-tracked path, but it succeeded"

    it "multiple untracked binary files in one go all persist after a single commitWorktree" $ do
      let result = fst <$> runChain (do
            writeFile "portrait.png" binaryBytes
            writeFile "cover.jpg" (BS.pack [0x00, 0x11, 0x22])
            commitWorktree
            p <- committedContent "portrait.png"
            c <- committedContent "cover.jpg"
            return (p, c))
      result `shouldBe` Right (binaryBytes, BS.pack [0x00, 0x11, 0x22])

    it "re-running commitWorktree with nothing changed doesn't add a redundant opaque commit" $ do
      let result = runChain (do
            writeFile "portrait.png" binaryBytes
            commitWorktree
            before <- chainIds
            commitWorktree
            after <- chainIds
            return (length before, length after))
      case result of
        Left err -> expectationFailure err
        Right ((before, after), _finalState) -> after `shouldBe` before

    -- The upload endpoint (Server.Writer.Branch.uploadFiles) reconciles
    -- via 'commitFiles', not the whole-tree 'commitWorktree' -- a binary
    -- upload needs the same persistence guarantee through that narrower
    -- path too, not just the general one.
    it "commitFiles (the scoped reconciler the upload endpoint uses) also persists a binary file" $ do
      let result = fst <$> runChain (do
            writeFile "portrait.png" binaryBytes
            commitFiles ["portrait.png"]
            committedContent "portrait.png")
      result `shouldBe` Right binaryBytes
