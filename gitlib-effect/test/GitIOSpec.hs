{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | End-to-end check of 'runGitIO' against a real, temporary git
-- repository.
--
-- 'GitHashSpec' only checks the hash *formula* in isolation against
-- @git hash-object@; it never runs 'runGitIO' itself. This exercises the
-- actual interpreter wiring -- 'WriteCommit'/'WriteObject' now return a
-- locally-computed hash instead of parsing git's stdout -- round-tripped
-- through a real write, a real persisted object, and a real read back by
-- that hash. If the self-computed hash didn't match what @git@ actually
-- persisted the object under, these reads would fail outright (unknown
-- revision) rather than silently returning wrong data.
module GitIOSpec (spec) where

import Polysemy
import Polysemy.Fail (Fail, runFail)
import Polysemy.Resource (Resource, runResource)
import Test.Hspec

import Runix.Cmd (Cmd, Cmds, cmdsIO, interpretCmd)
import Runix.Git
import TestTempRepo (withTempRepo)

-- | Same interpreter stack as 'Storyteller.Core.Runtime.runInfrastructure'
-- for the 'Git'/'Cmd "git"' portion, minus the unrelated effects (HTTP,
-- time, logging, ...) that stack also carries. 'Fail' is surfaced as a
-- plain 'IO' exception rather than through 'Runix.Logging.failLog', since
-- these specs have nothing to log to.
runInRepo :: FilePath -> Sem '[Git, Cmd "git", Cmds, Resource, Fail, Embed IO] a -> IO a
runInRepo repo action = do
  result <- runM . runFail . runResource . cmdsIO . interpretCmd @"git" . runGitIOPerCall repo $ action
  either (\e -> ioError (userError ("runGitIO: " <> e))) return result

spec :: Spec
spec = describe "runGitIO (real git subprocess interpreter)" $ do
  it "round-trips a blob written and read back by its self-computed hash" $
    withTempRepo $ \repo -> do
      content <- runInRepo repo $ do
        h <- writeBlob "hello self-hashed world"
        readBlob h
      content `shouldBe` "hello self-hashed world"

  it "round-trips a commit (self-hashed) whose tree (mktree-hashed) is read back correctly" $
    withTempRepo $ \repo -> do
      (treeHash, cd) <- runInRepo repo $ do
        h1   <- writeBlob "atom content"
        tree <- writeTree [BlobEntry "atom.md" h1]
        h2   <- writeCommit CommitData
                  { commitParents = []
                  , commitTree    = tree
                  , commitMessage = "first tick"
                  }
        cd   <- readCommit h2
        return (tree, cd)
      commitTree cd `shouldBe` treeHash
      commitMessage cd `shouldBe` "first tick"

  it "a ref written after a self-hashed commit resolves back to that exact commit" $
    withTempRepo $ \repo -> do
      (written, resolved) <- runInRepo repo $ do
        h1     <- writeBlob "root"
        tree   <- writeTree [BlobEntry "f" h1]
        h2     <- writeCommit CommitData
                    { commitParents = [], commitTree = tree, commitMessage = "root" }
        createRef (RefName "refs/heads/story/test") h2
        r <- resolveRef (RefName "refs/heads/story/test")
        return (h2, r)
      resolved `shouldBe` Just written

  it "a second commit's self-hashed parent link is followable back to the first" $
    withTempRepo $ \repo -> do
      parents <- runInRepo repo $ do
        h1 <- writeBlob "root"
        tree1 <- writeTree [BlobEntry "f" h1]
        root  <- writeCommit CommitData
                   { commitParents = [], commitTree = tree1, commitMessage = "root" }
        h2    <- writeBlob "rootchild"
        tree2 <- writeTree [BlobEntry "f" h2]
        child <- writeCommit CommitData
                   { commitParents = [root], commitTree = tree2, commitMessage = "child" }
        cd <- readCommit child
        return (commitParents cd, root)
      fst parents `shouldBe` [snd parents]

  it "isAncestorOfAny finds a target several generations back, including the head itself" $
    withTempRepo $ \repo -> do
      (root, mid, tip, unrelated) <- runInRepo repo $ do
        h1   <- writeBlob "root"
        tree <- writeTree [BlobEntry "f" h1]
        root <- writeCommit CommitData
                  { commitParents = [], commitTree = tree, commitMessage = "root" }
        mid  <- writeCommit CommitData
                  { commitParents = [root], commitTree = tree, commitMessage = "mid" }
        tip  <- writeCommit CommitData
                  { commitParents = [mid], commitTree = tree, commitMessage = "tip" }
        h2       <- writeBlob "other root"
        tree2    <- writeTree [BlobEntry "g" h2]
        unrelated <- writeCommit CommitData
                  { commitParents = [], commitTree = tree2, commitMessage = "unrelated" }
        return (root, mid, tip, unrelated)
      results <- runInRepo repo $ do
        hitsRoot      <- isAncestorOfAny [root] tip
        hitsMid       <- isAncestorOfAny [mid] tip
        hitsSelf      <- isAncestorOfAny [tip] tip
        missesUnrelated <- isAncestorOfAny [unrelated] tip
        return (hitsRoot, hitsMid, hitsSelf, missesUnrelated)
      results `shouldBe` (True, True, True, False)
