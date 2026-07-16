{-# LANGUAGE OverloadedStrings #-}

-- | 'Storage.Core.store' is supposed to cost what the one tick being
--   committed costs -- a spine of trees down to its own path -- never a
--   load and re-flush of the whole working tree; the same goes for a
--   rebase replaying a 'Binary'\/'Opaque' tick (whose exact tree
--   contribution 'Storage.Core' recovers via a structural 'treeDelta',
--   skipping every subtree the two commits share). Same approach as
--   "Storage.FoldIntoOpCountSpec": the load-bearing assertions are the
--   *equalities* -- the identical operation costs the exact same number
--   of physical store operations over a wide tree as over a narrow one,
--   which directly demonstrates independence from tree size instead of
--   merely staying under some threshold.
module Storage.StoreOpCountSpec (spec) where

import Prelude hiding (drop, readFile, writeFile)

import Control.Monad.State.Strict (lift)
import qualified Data.Text as T

import Test.Hspec

import Storage.Core
import Storage.Ops
import Storage.MockStore
import Storage.OpCounting

-- | @n@ files spread over @n@ directories -- a tree whose size and
--   directory count both grow with @n@, none of which the operation
--   under test has any business reading.
wideTree :: StoreM m => Int -> StoreT m ()
wideTree n = mapM_ file [1 .. n]
  where
    file i = addAtom ("dir" <> show i <> "/file.md") (T.pack ("content " <> show i))

shouldCostTheSame :: Either String OpCounts -> Either String OpCounts -> Expectation
shouldCostTheSame small big = case (small, big) of
  (Left err, _)       -> expectationFailure ("small-tree run failed: " <> err)
  (_, Left err)       -> expectationFailure ("big-tree run failed: " <> err)
  (Right s, Right b)  -> b `shouldBe` s

spec :: Spec
spec = do
  describe "store: committing one atom" $ do
    it "costs exactly the same over a wide tree as over a narrow one" $
      shouldCostTheSame
        (snd <$> runMeasuring (wideTree 3)  (addAtom "hot/story.md" "hello"))
        (snd <$> runMeasuring (wideTree 40) (addAtom "hot/story.md" "hello"))

    it "extending an existing file costs the same regardless of tree size" $
      shouldCostTheSame
        (snd <$> runMeasuring (wideTree 3  >> () <$ addAtom "hot/story.md" "a")
                              (addAtom "hot/story.md" "b"))
        (snd <$> runMeasuring (wideTree 40 >> () <$ addAtom "hot/story.md" "a")
                              (addAtom "hot/story.md" "b"))

  describe "store: a whole-file deletion marker" $
    it "costs exactly the same over a wide tree as over a narrow one" $
      shouldCostTheSame
        (snd <$> runMeasuring (wideTree 3  >> () <$ addAtom "hot/story.md" "x")
                              (deleteFile "hot/story.md"))
        (snd <$> runMeasuring (wideTree 40 >> () <$ addAtom "hot/story.md" "x")
                              (deleteFile "hot/story.md"))

  describe "rebase: replaying a Binary tick via treeDelta" $
    -- Deleting the atom right below a Binary tick forces 'at' to capture
    -- the binary's own tree contribution ('treeDelta') and reapply it
    -- ('storeWithDelta') -- the one replay path that used to load both
    -- commits' whole trees. The measured action finds its target by
    -- following head's own parent link (one read, independent of the
    -- tree) rather than smuggling an id out of the uninstrumented setup.
    it "costs exactly the same over a wide tree as over a narrow one" $ do
      let scenario n = do
            wideTree n
            _ <- addAtom "hot/doomed.md" "x"
            _ <- addBinary "img.png" "\137PNG-ish bytes"
            return ()
          deleteBelowBinary = do
            binary <- headHash
            cd     <- lift (readCommit binary)
            case commitParents cd of
              (doomed : _) -> deleteTick doomed
              []           -> fail "deleteBelowBinary: no parent under head"
      shouldCostTheSame
        (snd <$> runMeasuring (scenario 3)  deleteBelowBinary)
        (snd <$> runMeasuring (scenario 40) deleteBelowBinary)
