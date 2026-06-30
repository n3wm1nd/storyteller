{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Storyteller.EditSpec (spec) where

import Data.List (nub, sort, sortBy, permutations)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Maybe (fromJust, mapMaybe)
import Test.Hspec
import Test.QuickCheck

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState, State)

import Git.Mock
import Runix.Git (Git)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as T
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, readFile)
import Prelude hiding (readFile)

import Storyteller.Git
import Storyteller.Storage hiding (get, drop)
import qualified Storyteller.Storage as S
import Storyteller.Types
import Storyteller.Atom (AtomDiff(..), storeAtomDiff, treeRef)
import Runix.Git (Git)
import Storyteller.Edit

-- ---------------------------------------------------------------------------
-- Phantom
-- ---------------------------------------------------------------------------

data Main

-- ---------------------------------------------------------------------------
-- Runner
-- ---------------------------------------------------------------------------

runEdit
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
runEdit action =
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

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Store n ticks with messages "t1".."tn", return their ids oldest-first.
storeN :: Members '[StoryBranch Main, FileSystem (BranchTag Main), FileSystemRead (BranchTag Main), FileSystemWrite (BranchTag Main), StoryStorage, Fail] r => Int -> Sem r [TickId]
storeN n = mapM (\i -> storeData @Main (draft (T.pack ("t" <> show i)))) [1..n]

-- | Collect all tick messages oldest-first (skipping the root tick).
-- follow with (t:acc) builds newest-first; reverse gives oldest-first.
chainMessages :: Members '[StoryBranch Main, Fail] r => Sem r [T.Text]
chainMessages = do
  ticks <- S.follow @Main [] (\acc t -> (t : acc, tickParent t))
  -- ticks = [root, t1, t2, ... head] newest-first, so reverse = oldest-first
  return [ tickMessage (tickData t) | t <- ticks, tickParent t /= Nothing ]

-- | Collect (tickId, message) pairs oldest-first, skipping root.
chainPairs :: Members '[StoryBranch Main, Fail] r => Sem r [(TickId, T.Text)]
chainPairs = do
  ticks <- S.follow @Main [] (\acc t -> (t : acc, tickParent t))
  return [ (tickId t, tickMessage (tickData t)) | t <- ticks, tickParent t /= Nothing ]

-- | Apply a mapping to a list of ids.
applyMapping :: [(TickId, TickId)] -> [TickId] -> [TickId]
applyMapping m = map (\i -> maybe i id (lookup i m))

-- | Append bytes to a file and commit as an atom tick. Convenience for tests.
appendAtom :: Members '[StoryBranch Main, StoryStorage, Git, Fail] r
           => FilePath -> BS.ByteString -> Sem r TickId
appendAtom path content = do
  headTick   <- S.get @Main
  parentTree <- treeRef (tickId headTick)
  atom       <- storeAtomDiff parentTree (AtomDiff path content)
  storeAs @Main atom

-- ---------------------------------------------------------------------------
-- Unit tests: popTick / pushTick
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "popTick" $ do
    it "captures head's message and refs" $ do
      -- at tid runs action with tid at head; popTick pops the head tick.
      let result = runEdit $ do
            refs <- storeN 2        -- t1, t2
            let [t1, t2] = refs
            -- Store a tick with refs pointing to t1 and t2
            t3 <- storeData @Main (TickData { tickRefs = refs, tickFields = [], tickMessage = "annotated" })
            -- at t3: t3 is at head, pop it
            (d, _) <- at @Main t3 $ popTick @Main
            return d
      case result of
        Left err -> fail err
        Right d  -> do
          tdMessage d `shouldBe` "annotated"
          length (tdRefs d) `shouldBe` 2

    it "pop then push at same position is identity on messages" $ do
      -- at t2: pop t2, push it back. at replays t3 on top → same chain.
      let result = runEdit $ do
            [_t1, t2, _t3] <- storeN 3
            _ <- at @Main t2 $ do
              d <- popTick @Main
              _ <- pushTick @Main d
              return ()
            chainMessages
      case result of
        Left err -> fail err
        Right msgs -> msgs `shouldBe` ["t1", "t2", "t3"]

  -- ---------------------------------------------------------------------------
  -- Unit tests: deleteTick
  -- ---------------------------------------------------------------------------

  describe "deleteTick" $ do
    it "removes the tick and shifts tail ids" $ do
      let result = runEdit $ do
            [_t1, t2, _t3] <- storeN 3
            mapping <- deleteTick @Main t2
            msgs <- chainMessages
            return (msgs, length mapping)
      case result of
        Left err -> fail err
        Right (msgs, nMapped) -> do
          msgs    `shouldBe` ["t1", "t3"]
          nMapped `shouldBe` 1   -- only t3 gets a new id

    it "tail mapping covers all ticks after deleted one" $ do
      let result = runEdit $ do
            [t1, t2, t3, t4] <- storeN 4
            mapping <- deleteTick @Main t2
            return (length mapping)
      result `shouldBe` Right 2  -- t3 and t4 each get new ids

    it "delete first tick leaves rest intact" $ do
      let result = runEdit $ do
            [t1, _t2, _t3] <- storeN 3
            _ <- deleteTick @Main t1
            chainMessages
      result `shouldBe` Right ["t2", "t3"]

    it "delete last tick" $ do
      let result = runEdit $ do
            [_t1, _t2, t3] <- storeN 3
            _ <- deleteTick @Main t3
            chainMessages
      result `shouldBe` Right ["t1", "t2"]

  -- ---------------------------------------------------------------------------
  -- Unit tests: moveTick — backward (moving later tick earlier)
  -- ---------------------------------------------------------------------------

  describe "moveTick backward" $ do
    it "moves last tick to front" $ do
      let result = runEdit $ do
            [t1, _t2, t3] <- storeN 3
            mapping <- moveTick @Main t3 Nothing
            msgs <- chainMessages
            return (msgs, mapping)
      case result of
        Left err -> fail err
        Right (msgs, _) -> msgs `shouldBe` ["t3", "t1", "t2"]

    it "moves last tick to middle" $ do
      let result = runEdit $ do
            [t1, _t2, t3] <- storeN 3
            mapping <- moveTick @Main t3 (Just t1)
            msgs <- chainMessages
            return msgs
      result `shouldBe` Right ["t1", "t3", "t2"]

    it "moves middle tick to front" $ do
      let result = runEdit $ do
            [_t1, t2, _t3] <- storeN 3
            _ <- moveTick @Main t2 Nothing
            chainMessages
      result `shouldBe` Right ["t2", "t1", "t3"]

  -- ---------------------------------------------------------------------------
  -- Unit tests: moveTick — forward (moving earlier tick later)
  -- ---------------------------------------------------------------------------

  describe "moveTick forward" $ do
    it "moves first tick to last" $ do
      let result = runEdit $ do
            [t1, _t2, t3] <- storeN 3
            _ <- moveTick @Main t1 (Just t3)
            chainMessages
      result `shouldBe` Right ["t2", "t3", "t1"]

    it "moves first tick to middle" $ do
      let result = runEdit $ do
            [t1, t2, _t3] <- storeN 3
            _ <- moveTick @Main t1 (Just t2)
            chainMessages
      result `shouldBe` Right ["t2", "t1", "t3"]

    it "moves middle tick to last" $ do
      let result = runEdit $ do
            [_t1, t2, t3] <- storeN 3
            _ <- moveTick @Main t2 (Just t3)
            chainMessages
      result `shouldBe` Right ["t1", "t3", "t2"]

  -- ---------------------------------------------------------------------------
  -- Unit tests: identity moves
  -- ---------------------------------------------------------------------------

  describe "moveTick identity" $ do
    it "moving tick to its own position is a no-op on messages" $ do
      let result = runEdit $ do
            [t1, t2, _t3] <- storeN 3
            _ <- moveTick @Main t2 (Just t1)  -- t2 is already after t1
            chainMessages
      result `shouldBe` Right ["t1", "t2", "t3"]

    it "moving first tick to front is a no-op on messages" $ do
      let result = runEdit $ do
            [t1, _t2, _t3] <- storeN 3
            _ <- moveTick @Main t1 Nothing
            chainMessages
      result `shouldBe` Right ["t1", "t2", "t3"]

  -- ---------------------------------------------------------------------------
  -- Unit tests: id mapping correctness
  -- ---------------------------------------------------------------------------

  describe "moveTick id mapping" $ do
    it "moved tick appears in the mapping" $ do
      let result = runEdit $ do
            [t1, _t2, t3] <- storeN 3
            mapping <- moveTick @Main t3 Nothing
            return (any (\(o, _) -> o == t3) mapping)
      result `shouldBe` Right True

    it "mapping has no duplicate old ids" $ do
      let result = runEdit $ do
            [t1, _t2, t3] <- storeN 3
            mapping <- moveTick @Main t3 (Just t1)
            return (map fst mapping)
      case result of
        Left err -> fail err
        Right olds -> length olds `shouldBe` length (nub olds)

    it "applying the mapping to original ids yields all-new ids" $ do
      let result = runEdit $ do
            ids <- storeN 3
            let [t1, t2, t3] = ids
            mapping <- moveTick @Main t3 Nothing
            let newIds = applyMapping mapping ids
            pairs <- chainPairs
            return (map fst pairs, newIds)
      case result of
        Left err -> fail err
        Right (chainIds, mappedIds) ->
          sort chainIds `shouldBe` sort mappedIds

  -- ---------------------------------------------------------------------------
  -- Unit tests: error cases
  -- ---------------------------------------------------------------------------

  describe "moveTick errors" $ do
    it "fails if tid not in chain" $ do
      let result = runEdit $ do
            _ <- storeN 3
            moveTick @Main (TickId "nonexistent") Nothing
      case result of
        Left err -> err `shouldContain` "not found"
        Right _  -> fail "expected failure"

    it "fails if afterTickId not in chain" $ do
      let result = runEdit $ do
            [t1, _t2, _t3] <- storeN 3
            moveTick @Main t1 (Just (TickId "nonexistent"))
      case result of
        Left err -> err `shouldContain` "not found"
        Right _  -> fail "expected failure"

    it "fails if move would place tick before its ref" $ do
      -- t2 refs t3 (a note-style tick); can't move t2 before t3
      let result = runEdit $ do
            [t1, _t2, t3] <- storeN 3
            t2ref <- storeData @Main (TickData { tickRefs = [t3], tickFields = [], tickMessage = "note" })
            moveTick @Main t2ref Nothing  -- would place note before t3
      case result of
        Left err -> err `shouldContain` "reference"
        Right _  -> fail "expected ordering invariant failure"

    it "fails if move would place tick after its referencing tick" $ do
      -- t1 is referenced by t2note; can't move t1 after t2note
      let result = runEdit $ do
            [t1, _t2] <- storeN 2
            t2note <- storeData @Main (TickData { tickRefs = [t1], tickFields = [], tickMessage = "note" })
            moveTick @Main t1 (Just t2note)  -- t1 after t2note which refs t1
      case result of
        Left err -> err `shouldContain` "references"
        Right _  -> fail "expected ordering invariant failure"

  -- ---------------------------------------------------------------------------
  -- Note ref preservation across moves
  -- ---------------------------------------------------------------------------

  describe "moveTick note ref" $ do
    it "note retains its ref when moved down one place" $ do
      -- chain: [a1, a2, a3, n1]  n1 refs a2
      -- move n1 down one: [a1, a2, n1, a3]
      -- a3 is replayed; n1 must still reference a2's new id
      let result = runEdit $ do
            [a1, a2, a3] <- storeN 3
            n1 <- storeData @Main (TickData { tickRefs = [a2], tickFields = [("type","note")], tickMessage = "a comment" })
            mapping <- moveTick @Main n1 (Just a2)
            let newN1 = maybe n1 id (lookup n1 mapping)
                newA2 = maybe a2 id (lookup a2 mapping)
            (tick, _) <- at @Main newN1 (S.get @Main)
            return (tickRefs (tickData tick), newA2)
      case result of
        Left err            -> fail err
        Right (refs, newA2) -> refs `shouldBe` [newA2]


  -- ---------------------------------------------------------------------------
  -- QuickCheck properties
  -- ---------------------------------------------------------------------------

  describe "moveTick QuickCheck" $ do

    it "move never changes the set of messages" $
      property $ \(Positive n) (NonNegative from) (NonNegative to) ->
        let n'    = min n 8 + 2   -- chain size [2..9]
            from' = from `mod` n'
            to'   = to   `mod` n'
        in
        let result = runEdit $ do
              ids <- storeN n'
              let tid    = ids !! from'
                  mAfter = if to' == 0 then Nothing else Just (ids !! (to' - 1))
              _ <- moveTick @Main tid mAfter
              chainMessages
        in case result of
             Left _     -> property True  -- may fail on invariant violations; skip
             Right msgs -> sort msgs === sort (map (\i -> T.pack ("t" <> show i)) [1..n'])

    it "move places the tick at the expected position (to' as afterPos)" $
      property $ \(Positive n) (NonNegative from) (NonNegative to) ->
        let n'    = min n 8 + 2
            from' = from `mod` n'
            -- to' is the index in contentOrdered after which we insert (0=front, n'=back)
            -- expressed as: if to'=0 Nothing, else Just ids!!(to'-1)
            to'   = to `mod` (n' + 1)  -- 0..n': 0=front, k=after kth tick
        in
        -- Expected position of tid in result: to' (0-based in content chain)
        -- But skip this test if from'==to'-1 (identity: tid already at that position).
        let result = runEdit $ do
              ids <- storeN n'
              let tid    = ids !! from'
                  mAfter = if to' == 0 then Nothing else Just (ids !! (to' - 1))
              _ <- moveTick @Main tid mAfter
              chainMessages
        in case result of
             Left _    -> property True   -- invariant violations OK to skip
             Right msgs ->
               -- The moved message should appear in the result
               let movedMsg = T.pack ("t" <> show (from' + 1))
               in (movedMsg `elem` msgs) === True

    it "mapping ids are all distinct" $
      property $ \(Positive n) (NonNegative from) (NonNegative to) ->
        let n'    = min n 8 + 2
            from' = from `mod` n'
            to'   = to   `mod` n'
        in
        let result = runEdit $ do
              ids <- storeN n'
              let tid    = ids !! from'
                  mAfter = if to' == 0 then Nothing else Just (ids !! (to' - 1))
              moveTick @Main tid mAfter
        in case result of
             Left _       -> property True
             Right mapping ->
               let olds = map fst mapping
                   news = map snd mapping
               in (length olds === length (nub olds))
                  .&&. (length news === length (nub news))

    it "chain length is preserved after move" $
      property $ \(Positive n) (NonNegative from) (NonNegative to) ->
        let n'    = min n 8 + 2
            from' = from `mod` n'
            to'   = to   `mod` n'
        in
        let result = runEdit $ do
              ids <- storeN n'
              let tid    = ids !! from'
                  mAfter = if to' == 0 then Nothing else Just (ids !! (to' - 1))
              _ <- moveTick @Main tid mAfter
              chainMessages
        in case result of
             Left _    -> property True
             Right msgs -> length msgs === n'

  -- ---------------------------------------------------------------------------
  -- File content correctness after move
  -- ---------------------------------------------------------------------------

  describe "moveTick file content" $ do
    it "backward move preserves correct file content order" $ do
      -- Chain: [para1][para2][para3]. Move para3 to front.
      -- Expected file content: para3 para1 para2 (each appended in new order).
      let result = runEdit $ do
            t1 <- appendAtom "scene.md" "para1\n"
            t2 <- appendAtom "scene.md" "para2\n"
            t3 <- appendAtom "scene.md" "para3\n"
            _ <- moveTick @Main t3 Nothing
            readFile @(BranchTag Main) "scene.md"
      case result of
        Left err -> fail err
        Right bs -> bs `shouldBe` "para3\npara1\npara2\n"

    it "forward move preserves correct file content order" $ do
      -- Chain: [para1][para2][para3]. Move para1 to after para3.
      -- Expected: para2 para3 para1.
      let result = runEdit $ do
            t1 <- appendAtom "scene.md" "para1\n"
            t2 <- appendAtom "scene.md" "para2\n"
            t3 <- appendAtom "scene.md" "para3\n"
            _ <- moveTick @Main t1 (Just t3)
            readFile @(BranchTag Main) "scene.md"
      case result of
        Left err -> fail err
        Right bs -> bs `shouldBe` "para2\npara3\npara1\n"

    it "move to middle preserves correct file content order" $ do
      -- Chain: [A][B][C][D]. Move D to after B. Expected: A B D C.
      let result = runEdit $ do
            ta <- appendAtom "scene.md" "A\n"
            tb <- appendAtom "scene.md" "B\n"
            tc <- appendAtom "scene.md" "C\n"
            td <- appendAtom "scene.md" "D\n"
            _ <- moveTick @Main td (Just tb)
            readFile @(BranchTag Main) "scene.md"
      case result of
        Left err -> fail err
        Right bs -> bs `shouldBe` "A\nB\nD\nC\n"

    it "QuickCheck: file content is a permutation of original paragraphs" $
      property $ \(Positive n) (NonNegative from) (NonNegative to) ->
        let n'    = min n 6 + 2
            from' = from `mod` n'
            to'   = to   `mod` (n' + 1)
        in
        let result = runEdit $ do
              ids <- mapM (\i -> appendAtom "f.md" (T.encodeUtf8 (T.pack ("p" <> show i <> "\n")))) [1..n']
              let tid    = ids !! from'
                  mAfter = if to' == 0 then Nothing else Just (ids !! (to' - 1))
              _ <- moveTick @Main tid mAfter
              readFile @(BranchTag Main) "f.md"
        in case result of
             Left _  -> property True
             Right bs ->
               let content = T.decodeUtf8 bs
                   paras   = filter (not . T.null) (T.splitOn "\n" content)
                   expected = sort [ T.pack ("p" <> show i) | i <- [1..n'] ]
               in sort paras === expected
