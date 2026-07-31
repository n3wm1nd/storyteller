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
import GHC.Clock (getMonotonicTime)

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

  describe "commitFile: a near-HEAD edit on a path with a long history of its own" $ do
    -- Unlike 'opsFor', the growing history here is @path@'s *own* atoms,
    -- not an unrelated noise path -- 'atomTrackedAmong'/'hasAnyAtom'
    -- (see "Storage.Query") only stops once it finds *this path's* first
    -- atom, so a long unrelated history never exercises it; a long
    -- same-path history is the case that actually would.
    let manyAtoms n   = map (\i -> T.pack ("p" <> show i <> " ")) [1 .. n]
        edited n      = T.concat (manyAtoms n) <> "TAIL EDIT"
        opsForSamePath n = snd <$> runMeasuring
          (mapM_ (addAtom path) (manyAtoms n))
          (writeFile path (TE.encodeUtf8 (edited n)) >> commitFile path)

    it "a tail edit costs the same whether path itself has a little history or a lot" $
      shouldCostTheSame (opsForSamePath 5) (opsForSamePath 2000)

  describe "commitFile: widening a blank-line run right at HEAD" $ do
    -- Mimics a real raw-editor save reported to take seconds even on a
    -- small project, "only touching atoms close to the head": atoms
    -- shaped like 'Storyteller.Common.Splitter.byParagraph' actually
    -- produces (each non-final atom's own stored text ends in the
    -- "\n\n" delimiter run that split it from the next paragraph), then
    -- one extra blank line inserted into the delimiter run between the
    -- *last two* paragraphs -- i.e. right at HEAD, however large the
    -- file grows below it.
    --
    -- This turns out already cheap: 'resolveLocal' settles it in one
    -- 'applyLocalEdit' at the very first atom it looks at (see the
    -- prefix-match branch), so this is a regression guard on that
    -- property, not a reproduction of the reported slowdown -- an
    -- *earlier* version of this same scenario, widening the delimiter
    -- between the *first* two paragraphs instead (i.e. right at the
    -- lifetime's root), genuinely was superlinear, but that is
    -- 'Storage.Core.at''s own inherent replay-every-ancestor cost for an
    -- edit far from head (every tick above it needs a new hash), not a
    -- reconciliation bug -- see git history for that version if the
    -- root-adjacent case ever needs its own guard.
    --
    -- Not an op-count assertion (that's silent to an in-memory
    -- longestCommonSubstring blowup -- no store op is a proxy for CPU
    -- spent building its O(n*m) DP table) -- a wall-clock budget instead,
    -- generous enough to be robust to machine noise but tight enough to
    -- catch genuine superlinear-in-file-length blowup.
    let paragraph n = T.pack ("para" <> show n <> ": ") <> T.replicate 200 "x" <> "\n\n"
        paragraphs n = map paragraph [1 .. n]
        widened n = T.concat (map paragraph [1 .. n - 2]) <> paragraph (n - 1) <> "\n\n" <> paragraph n
        -- The store here is pure ('Either String', no 'IO'), so
        -- 'commitFile' alone can't be timed from partway through one
        -- monadic computation the way 'PerfCascade.hs's
        -- 'buildScenario'\/'timedEdit' split does (that one reuses an
        -- already-built 'GitState' because 'Git.Mock' runs over real
        -- 'IO'\/'Polysemy' state). Instead: time setup-alone, then time
        -- setup-plus-the-real-edit, both as fresh, independent
        -- 'runMeasuring' calls -- setup is deterministic, so the
        -- difference isolates the edit's own cost, the same subtraction
        -- 'RealGitPerf.hs' does explicitly with two timestamps around one
        -- shared run, just with two runs instead of a shared one.
        setupOnly n = fst <$> runMeasuring
          (mapM_ (addAtom path) (paragraphs n)) (return ())
        setupPlusEdit n = fst <$> runMeasuring
          (mapM_ (addAtom path) (paragraphs n))
          (writeFile path (TE.encodeUtf8 (widened n)) >> commitFile path)
        timedRun m = do
          t0 <- getMonotonicTime
          case m of
            Left err -> t0 `seq` return (Left err)
            Right r  -> r `seq` do
              t1 <- getMonotonicTime
              return (Right (t1 - t0))
        timedWiden n = do
          rSetup <- timedRun (setupOnly n)
          rTotal <- timedRun (setupPlusEdit n)
          return ((,) <$> rSetup <*> rTotal)

    it "costs roughly linearly, not quadratically or worse, in file length" $ do
      -- 4x the paragraphs (and so ~4x the total file length): a linear
      -- (or even O(n log n)) algorithm finishes comfortably inside a
      -- budget scaled a small constant past 4x the smaller run's own
      -- time; an O(fileLength^2)-per-atom blowup like an unbounded
      -- longestCommonSubstring match against the whole remaining target
      -- overshoots that budget by a wide margin.
      rSmall <- timedWiden 100
      rBig   <- timedWiden 400
      case (rSmall, rBig) of
        (Left err, _) -> expectationFailure ("small run failed: " <> err)
        (_, Left err) -> expectationFailure ("big run failed: " <> err)
        (Right (tSetupSmall, tTotalSmall), Right (tSetupBig, tTotalBig)) ->
          -- Generous ceiling (8x, for a 4x size step) -- still well below
          -- what an O(n^2)-ish blowup (16x, then far worse once GHC's
          -- Array construction and Char list unpacking join in) produces
          -- in practice; a floor on the small run guards against a
          -- degenerate "both ran in noise" pass hiding a real regression.
          let tSmall = tTotalSmall - tSetupSmall
              tBig   = tTotalBig - tSetupBig
          in (tBig, tSmall) `shouldSatisfy` \(b, s) -> b < max 0.05 (s * 8)

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
