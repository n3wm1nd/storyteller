{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Correctness check for 'Runix.Git.Batch': objects written through the
-- already-tested 'runGitIO' write path (see 'GitIOSpec') must read back
-- through the persistent @cat-file --batch@ reader with the same type and
-- content a direct one-shot @git cat-file@ call would report.
module GitBatchSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail (Fail, runFail)
import Test.Hspec

import Runix.Cmd (Cmd, Cmds, cmdsIO, interpretCmd)
import Runix.Git
import Runix.Git.Batch
import TestTempRepo (withTempRepo)

runInRepo :: FilePath -> Sem '[Git, Cmd "git", Cmds, Fail, Embed IO] a -> IO a
runInRepo repo action = do
  result <- runM . runFail . cmdsIO . interpretCmd @"git" . runGitIO repo $ action
  either (\e -> ioError (userError ("runGitIO: " <> e))) return result

spec :: Spec
spec = describe "Runix.Git.Batch.readBatch" $ do
  it "reads a written blob back with type \"blob\" and its exact content" $
    withTempRepo $ \repo -> do
      hash <- runInRepo repo (writeBlob "hello batch world")
      result <- withBatchReader repo $ \br -> readBatch br hash
      result `shouldBe` Just ("blob", "hello batch world")

  it "reads a written commit back with type \"commit\", carrying its tree hash and message" $
    withTempRepo $ \repo -> do
      (hash, treeHash) <- runInRepo repo $ do
        h1   <- writeBlob "atom content"
        tree <- writeTree [BlobEntry "atom.md" h1]
        h2   <- writeCommit CommitData
                  { commitParents = [], commitTree = tree, commitMessage = "first tick" }
        return (h2, tree)
      result <- withBatchReader repo $ \br -> readBatch br hash
      case result of
        Just ("commit", content) -> do
          content `shouldSatisfy` BS.isInfixOf (BS8.pack ("tree " <> T.unpack (unObjectHash treeHash)))
          content `shouldSatisfy` BS.isInfixOf "first tick"
        other -> expectationFailure ("unexpected result: " <> show other)

  it "reports Nothing for a hash that was never written" $
    withTempRepo $ \repo -> do
      result <- withBatchReader repo $ \br ->
        readBatch br (ObjectHash "0000000000000000000000000000000000000000")
      result `shouldBe` Nothing

  it "serves several reads through the same persistent process, in request order" $
    withTempRepo $ \repo -> do
      let mkContent i = "content number " <> BS8.pack (show (i :: Int))
      hashes  <- runInRepo repo (mapM (writeBlob . mkContent) [1 .. 20])
      results <- withBatchReader repo $ \br -> mapM (readBatch br) hashes
      results `shouldBe` map (\i -> Just ("blob", mkContent i)) [1 .. 20]
