{-# LANGUAGE OverloadedStrings #-}

-- | 'Storage.Reconcile.commitFile' (via its core, @foldInto@) is
--   supposed to cost about what the actual edit costs, not the size of
--   the graph around it -- see that module's own Haddock. Content and
--   final-atom assertions ("Storage.CommitWorktreeSpec") already check
--   it does the *right* thing; this checks it does *little enough* of it,
--   using 'Storage.OpCounting' to intercept the physical store operations
--   a run actually performs, rather than inferring cost from wall-clock
--   time.
--
--   The load-bearing assertions here are the *equalities*: the same edit
--   costs the exact same number of operations whether it's preceded by a
--   handful of unrelated ticks or thousands of them. That's a much
--   stronger claim than "stays under some threshold" -- it directly
--   demonstrates independence from graph size, the property the whole
--   rewrite exists for, rather than merely being consistent with it.
module Storage.FoldIntoOpCountSpec (spec) where

import Prelude hiding (drop, readFile, writeFile, appendFile)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import Test.Hspec

import Storage.Core
import Storage.Ops
import Storage.MockStore
import Storage.OpCounting

path :: FilePath
path = "story.md"

-- | @n@ unrelated atoms on a different path -- a "busy graph" the real
--   edit under test has no reason to ever look at.
noise :: StoreM m => Int -> StoreT m ()
noise n = mapM_ (\i -> addAtom "noise.md" (T.pack ("n" <> show i))) [1 .. n]

-- | A fresh chain, @n@ unrelated ticks, then @path@ built from @atoms@
--   (oldest first) and its ambient content set to @target@ -- all of that
--   is setup, excluded from the count. What's measured is 'commitFile'
--   alone, reconciling @path@ to @target@.
opsFor :: Int -> [Text] -> Text -> Either String OpCounts
opsFor n atoms target = snd <$> runMeasuring
  (do noise n
      mapM_ (addAtom path) atoms
      writeFile path (TE.encodeUtf8 target))
  (commitFile path)

-- | @big@ is the "actual" side and @small@ the "expected" one, so a
--   failure reads the intuitive way round: "expected (the small-graph
--   baseline) X, but got (the huge-graph run) Y" -- a graph-size
--   dependency shows up as the big run costing *more* than expected, not
--   the other way round.
shouldCostTheSame :: Either String OpCounts -> Either String OpCounts -> Expectation
shouldCostTheSame small big = case (small, big) of
  (Left err, _)      -> expectationFailure ("small-graph run failed: " <> err)
  (_, Left err)       -> expectationFailure ("big-graph run failed: " <> err)
  (Right s, Right b)  -> b `shouldBe` s

spec :: Spec
spec = do
  describe "foldInto: a genuine no-op" $ do
    it "writes nothing when the ambient content already matches" $
      case opsFor 20 ["hello ", "world"] "hello world" of
        Left err     -> expectationFailure err
        Right counts -> ocWrites counts `shouldBe` 0

    it "costs exactly the same whether preceded by a little history or a lot" $
      shouldCostTheSame
        (opsFor 5    ["hello ", "world"] "hello world")
        (opsFor 2000 ["hello ", "world"] "hello world")

  describe "foldInto: a tail append" $ do
    it "writes only a small, bounded number of objects" $
      case opsFor 20 ["hello ", "world"] "hello world!!!" of
        Left err     -> expectationFailure err
        Right counts -> ocWrites counts `shouldSatisfy` (<= 6)

    it "costs exactly the same whether preceded by a little history or a lot" $
      shouldCostTheSame
        (opsFor 5    ["hello ", "world"] "hello world!!!")
        (opsFor 2000 ["hello ", "world"] "hello world!!!")

  describe "foldInto: a change confined to one middle atom stops at its own boundary" $ do
    let atoms    = ["aaaa", "bbbb", "cccc"]
        edited   = "aaaaBBBBcccc"     -- middle atom's content changes
        dropped  = "aaaacccc"         -- middle atom disappears entirely
        inserted = "aaaaNEWbbbbcccc"  -- new content spliced in beside it, itself untouched

    it "an in-place edit costs exactly the same over a small or huge graph" $
      shouldCostTheSame (opsFor 5 atoms edited) (opsFor 2000 atoms edited)

    it "a drop costs exactly the same over a small or huge graph" $
      shouldCostTheSame (opsFor 5 atoms dropped) (opsFor 2000 atoms dropped)

    it "an insertion costs exactly the same over a small or huge graph" $
      shouldCostTheSame (opsFor 5 atoms inserted) (opsFor 2000 atoms inserted)

    it "none of them read anywhere near as much as the graph actually has ticks" $
      case opsFor 2000 atoms edited of
        Left err     -> expectationFailure err
        Right counts -> ocReads counts `shouldSatisfy` (< 50)

  describe "commitWorktree: untracked (binary) paths share one tracking walk" $
    -- A path with no atom history can only be answered by walking to
    -- root; what must *not* happen is one such walk per path
    -- ('atomTrackedAmong' batches them into one). The walk itself is
    -- inherently O(chain), so the equality here is on the *marginal*
    -- cost: six extra binary paths cost exactly the same handful of
    -- extra operations over a huge chain as over a tiny one.
    it "the marginal cost of six extra binary paths is independent of chain length" $ do
      let run n j = snd <$> runMeasuring
            (do noise n
                mapM_ (\i -> writeFile ("bin" <> show (i :: Int) <> ".dat")
                               (BS.pack [0xff, 0xfe, fromIntegral i]))
                      [1 .. j])
            commitWorktree
          marginal n = (\small big -> (ocReads big - ocReads small, ocWrites big - ocWrites small))
                         <$> run n 2 <*> run n 8
      case (marginal 5, marginal 2000) of
        (Right m5, Right m2000) -> m2000 `shouldBe` m5
        (a, b)                  -> expectationFailure ("a run failed: " <> show (a, b))
