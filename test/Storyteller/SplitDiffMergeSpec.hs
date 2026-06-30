{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for the split-diff-merge pure logic.
--
-- The core invariant: for any sequence of appends across any set of files,
-- 'computeBlocks' correctly identifies the new content to be committed,
-- and reconstructing the working tree by applying those blocks to the
-- committed history produces the original working tree.
module Storyteller.SplitDiffMergeSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl', nub, sort)

import qualified Data.Text as T

import Test.Hspec
import Test.QuickCheck

import Storyteller.Types (TickId(..))
import Storyteller.Edit
  (computeBlocks, deriveHistory, blocksFromTimeline, AppendBlock(..), DiffError(..))

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

mkTick :: Int -> TickId
mkTick n = TickId ("tick-" <> T.pack (show n))

-- Build a 'FileHistory' from a list of (tickId, file→content) snapshots.
buildHistory :: [(TickId, Map FilePath BS.ByteString)] -> Map FilePath [(TickId, Int)]
buildHistory = deriveHistory

-- Apply a list of AppendBlocks to a base state to reconstruct the working tree.
applyBlocksToState
  :: Map FilePath BS.ByteString      -- ^ committed head state
  -> [AppendBlock]
  -> Map FilePath BS.ByteString
applyBlocksToState base blocks = foldl' applyOne base blocks
  where
    applyOne m (AppendBlock file _ content) =
      Map.insertWith (\new old -> old <> new) file content m

-- ---------------------------------------------------------------------------
-- Unit tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  quickCheckSpecs
  describe "computeBlocks / no-op" $ do
    it "returns no blocks when working tree matches committed head" $ do
      let history = buildHistory
            [ (mkTick 1, Map.singleton "a.txt" "hello") ]
          working = Map.singleton "a.txt" "hello"
      computeBlocks history working `shouldBe` Right []

    it "returns no blocks for an empty working tree" $ do
      let history = buildHistory [(mkTick 1, Map.singleton "a.txt" "hello")]
          working = Map.empty
      computeBlocks history working `shouldBe` Right []

  describe "computeBlocks / single file appends" $ do
    it "detects a simple suffix append" $ do
      let history = buildHistory
            [ (mkTick 1, Map.singleton "a.txt" "hello") ]
          working = Map.singleton "a.txt" "hello world"
      case computeBlocks history working of
        Left err -> expectationFailure (show err)
        Right blocks -> do
          length blocks `shouldBe` 1
          blockFile (head blocks) `shouldBe` "a.txt"
          blockContent (head blocks) `shouldBe` " world"
          blockAfterTick (head blocks) `shouldBe` mkTick 1

    it "detects two sequential appends in separate ticks" $ do
      let history = buildHistory
            [ (mkTick 1, Map.singleton "a.txt" "hello")
            , (mkTick 2, Map.singleton "a.txt" "hello world")
            ]
          working = Map.singleton "a.txt" "hello world!!!"
      case computeBlocks history working of
        Left err -> expectationFailure (show err)
        Right blocks -> do
          length blocks `shouldBe` 1
          blockContent (head blocks) `shouldBe` "!!!"
          blockAfterTick (head blocks) `shouldBe` mkTick 2

  describe "computeBlocks / new files" $ do
    it "returns no blocks for a file with no history (caller handles plain Store)" $ do
      let history = Map.empty
          working = Map.singleton "new.txt" "brand new content"
      computeBlocks history working `shouldBe` Right []

  describe "computeBlocks / modification detection" $ do
    it "fails if working content is shorter than committed" $ do
      let history = buildHistory
            [ (mkTick 1, Map.singleton "a.txt" "hello world") ]
          working = Map.singleton "a.txt" "hello"
      case computeBlocks history working of
        Left (ModificationDetected "a.txt" 11 5) -> return ()
        other -> expectationFailure ("expected ModificationDetected, got: " <> show other)

  describe "blocksFromTimeline" $ do
    it "suffix after last tick" $ do
      let timeline = [(mkTick 1, 5)]
          working  = BSC.pack "helloworld"
      case blocksFromTimeline "f" timeline working of
        Right [AppendBlock "f" t c] -> do
          t `shouldBe` mkTick 1
          c `shouldBe` "world"
        other -> expectationFailure (show other)

    it "working tree equals committed — no blocks" $ do
      let timeline = [(mkTick 1, 5)]
          working  = BSC.pack "hello"
      blocksFromTimeline "f" timeline working `shouldBe` Right []

    it "working tree shorter than committed — no blocks (caller detects)" $ do
      let timeline = [(mkTick 1, 10)]
          working  = BSC.pack "hello"
      blocksFromTimeline "f" timeline working `shouldBe` Right []

-- ---------------------------------------------------------------------------
-- QuickCheck properties
-- ---------------------------------------------------------------------------

-- | An append-only edit history for one file:
--   a non-empty list of byte sequences, each strictly longer than the previous.
newtype AppendHistory = AppendHistory [(TickId, BS.ByteString)]
  deriving (Show)

instance Arbitrary AppendHistory where
  arbitrary = do
    n      <- choose (1, 6)
    chunks <- vectorOf n (BSC.pack <$> listOf1 (choose ('a', 'z')))
    let snapshots = tail $ scanl (<>) "" chunks
        ticks     = map mkTick [1..n]
    return $ AppendHistory (zip ticks snapshots)

  shrink (AppendHistory xs) =
    [ AppendHistory (take n xs) | n <- [1..length xs - 1] ]

-- | Convert an AppendHistory into deriveHistory input format.
toSnapshots :: AppendHistory -> [(TickId, Map FilePath BS.ByteString)]
toSnapshots (AppendHistory xs) = [(tid, Map.singleton "f" content) | (tid, content) <- xs]

newtype ArbBytes = ArbBytes BS.ByteString deriving (Show)

instance Arbitrary ArbBytes where
  arbitrary = ArbBytes . BSC.pack <$> listOf (choose ('a', 'z'))
  shrink (ArbBytes bs) = [ArbBytes (BSC.pack s) | s <- shrink (BSC.unpack bs)]

-- | The key roundtrip property:
--   given a committed history and a working tree that is a suffix-extension of the head,
--   computeBlocks identifies the right content, and applying the blocks to the head
--   produces the working tree.
prop_roundtrip :: AppendHistory -> ArbBytes -> Bool
prop_roundtrip hist@(AppendHistory xs) (ArbBytes extra) =
  let snapshots     = toSnapshots hist
      history       = deriveHistory snapshots
      headContent   = snd (last xs)
      workingContent = headContent <> extra
      working       = Map.singleton "f" workingContent
  in case computeBlocks history working of
    Left _  -> BS.null extra  -- only valid failure: empty extra (no change)
    Right blocks ->
      let reconstructed = applyBlocksToState (Map.singleton "f" headContent) blocks
      in Map.lookup "f" reconstructed == Just workingContent

-- | Identity: if working tree equals committed head, no blocks are emitted.
prop_noChangeNoBlocks :: AppendHistory -> Bool
prop_noChangeNoBlocks hist@(AppendHistory xs) =
  let snapshots = toSnapshots hist
      history   = deriveHistory snapshots
      working   = Map.singleton "f" (snd (last xs))
  in computeBlocks history working == Right []

-- | Shrinkage: if working tree is shorter than committed, we get ModificationDetected.
prop_shrinkFails :: AppendHistory -> Positive Int -> Property
prop_shrinkFails hist@(AppendHistory xs) (Positive drop_) =
  let snapshots   = toSnapshots hist
      history     = deriveHistory snapshots
      headContent = snd (last xs)
      n           = BS.length headContent
      truncated   = BS.take (max 0 (n - min drop_ n)) headContent
  in n > 0 ==>
    BS.length truncated < n ==>
      case computeBlocks history (Map.singleton "f" truncated) of
        Left (ModificationDetected _ _ _) -> True
        _                                 -> False

-- | Blocks are non-empty when extra content exists.
prop_extraProducesBlocks :: AppendHistory -> Property
prop_extraProducesBlocks hist@(AppendHistory xs) =
  forAll (BSC.pack <$> listOf1 (choose ('a', 'z'))) $ \extra ->
    let snapshots = toSnapshots hist
        history   = deriveHistory snapshots
        working   = Map.singleton "f" (snd (last xs) <> extra)
    in case computeBlocks history working of
      Right blocks -> not (null blocks)
      Left _       -> False

-- | deriveHistory is monotone: byte lengths are non-decreasing.
prop_historyMonotone :: AppendHistory -> Bool
prop_historyMonotone hist =
  let snapshots = toSnapshots hist
      history   = deriveHistory snapshots
  in case Map.lookup "f" history of
    Nothing -> False
    Just timeline ->
      let lengths = map snd timeline
      in and (zipWith (<=) lengths (tail lengths))

quickCheckSpecs :: Spec
quickCheckSpecs = describe "QuickCheck" $ do
  it "roundtrip: applying blocks to head reproduces working tree" $
    property prop_roundtrip
  it "no-change: identical working tree produces no blocks" $
    property prop_noChangeNoBlocks
  it "shrink: shorter working tree fails with ModificationDetected" $
    property prop_shrinkFails
  it "extra content always produces at least one block" $
    property prop_extraProducesBlocks
  it "history byte lengths are non-decreasing" $
    property prop_historyMonotone
