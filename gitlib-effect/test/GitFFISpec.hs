{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Two layers of checks for the libgit2 FFI interpreter:
--
-- * Direct checks of 'Runix.Git.FFI's raw bindings against a real repo --
--   catches marshalling bugs (oid encode/decode, error-code handling) in
--   isolation, independent of the 'Runix.Git.Git' effect machinery.
-- * A differential suite running the same operation sequence through
--   'Runix.Git.runGitIOPerCall' (the real @git@ subprocess interpreter)
--   and 'Runix.Git.runGitFFIPerCall' (this one), asserting identical
--   results -- the same "validated against the real thing" discipline
--   'GitHashSpec'\/'GitStoreSpec' already use, one level up.
module GitFFISpec (spec) where

import Polysemy
import Polysemy.Fail (Fail, runFail)
import Polysemy.Resource (Resource, runResource)
import qualified Data.Set as Set
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Runix.Cmd (Cmd, Cmds, cmdsIO, interpretCmd)
import Runix.Git
import qualified Runix.Git.FFI as FFI
import Runix.Git.Hash (hashObject, ObjectKind(Blob))

runViaCLI :: FilePath -> Sem '[Git, Cmd "git", Cmds, Resource, Fail, Embed IO] a -> IO a
runViaCLI repo action = do
  result <- runM . runFail . runResource . cmdsIO . interpretCmd @"git" . runGitIOPerCall repo $ action
  either (\e -> ioError (userError ("runGitIOPerCall: " <> e))) return result

runViaFFI :: FilePath -> Sem '[Git, Resource, Fail, Embed IO] a -> IO a
runViaFFI repo action = do
  result <- runM . runFail . runResource . runGitFFIPerCall repo $ action
  either (\e -> ioError (userError ("runGitFFIPerCall: " <> e))) return result

-- | Runs the same polymorphic scenario through both interpreters, each
-- against its own fresh repo under @parent@, and returns both results for
-- comparison -- the shared shape every differential test below uses so
-- the scenario itself is written once.
runBoth
  :: FilePath
  -> (forall r. Members '[Git, Fail] r => Sem r a)
  -> IO (a, a)
runBoth parent scenario = do
  cli <- runViaCLI (parent </> "cli-repo") scenario
  ffi <- runViaFFI (parent </> "ffi-repo") scenario
  return (cli, ffi)

spec :: Spec
spec = do
  describe "Runix.Git.FFI (raw bindings)" $ do
    it "initializes libgit2 without error" $
      FFI.libgit2Init `shouldReturn` ()

    it "creates a bare repo at construction time when the path doesn't exist yet" $
      withSystemTempDirectory "gitlib-effect-ffi-spec" $ \parent -> do
        let repo = parent </> "repo"
        existsBefore <- doesDirectoryExist repo
        FFI.withRepository repo (const (return ()))
        existsAfter <- doesDirectoryExist (repo </> "objects")
        existsBefore `shouldBe` False
        existsAfter `shouldBe` True

    it "round-trips a blob written and read back by its libgit2-computed hash, matching the self-computed formula" $
      withSystemTempDirectory "gitlib-effect-ffi-spec" $ \parent -> do
        let repo = parent </> "repo"
            content = "hello from libgit2 FFI"
        (hash, readBack) <- FFI.withRepository repo $ \r -> do
          h <- FFI.writeBlob r content
          c <- FFI.readBlob r h
          return (h, c)
        readBack `shouldBe` content
        hash `shouldBe` hashObject Blob content

    it "resolves a nonexistent ref to Nothing" $
      withSystemTempDirectory "gitlib-effect-ffi-spec" $ \parent -> do
        let repo = parent </> "repo"
        result <- FFI.withRepository repo $ \r -> FFI.resolveRef r "refs/heads/does-not-exist"
        result `shouldBe` Nothing

  describe "runGitFFIPerCall vs runGitIOPerCall (differential)" $ do
    it "both bootstrap a fresh path into a repo a blob can be written to and read back from" $
      withSystemTempDirectory "gitlib-effect-ffi-diff" $ \parent -> do
        let repoCLI = parent </> "cli-repo"
            repoFFI = parent </> "ffi-repo"
            content = "differential test content"
        hashCLI <- runViaCLI repoCLI (writeBlob content >>= \h -> readBlob h >> return h)
        hashFFI <- runViaFFI repoFFI (writeBlob content >>= \h -> readBlob h >> return h)
        hashFFI `shouldBe` hashCLI

    it "both round-trip identical content to an identical hash" $
      withSystemTempDirectory "gitlib-effect-ffi-diff" $ \parent -> do
        let repoCLI = parent </> "cli-repo"
            repoFFI = parent </> "ffi-repo"
            content = "same bytes, both interpreters"
        (hashCLI, readCLI) <- runViaCLI repoCLI $ do
          h <- writeBlob content
          c <- readBlob h
          return (h, c)
        (hashFFI, readFFI) <- runViaFFI repoFFI $ do
          h <- writeBlob content
          c <- readBlob h
          return (h, c)
        hashFFI `shouldBe` hashCLI
        readFFI `shouldBe` readCLI
        readCLI `shouldBe` content

    it "both resolve a nonexistent ref to Nothing" $
      withSystemTempDirectory "gitlib-effect-ffi-diff" $ \parent -> do
        let repoCLI = parent </> "cli-repo"
            repoFFI = parent </> "ffi-repo"
            ref = RefName "refs/heads/does-not-exist"
        resolvedCLI <- runViaCLI repoCLI (resolveRef ref)
        resolvedFFI <- runViaFFI repoFFI (resolveRef ref)
        resolvedFFI `shouldBe` resolvedCLI
        resolvedFFI `shouldBe` Nothing

    it "both resolve a ref written by the CLI interpreter to the same hash" $
      withSystemTempDirectory "gitlib-effect-ffi-diff" $ \parent -> do
        let repo = parent </> "repo"
            content = "shared repo, cross-interpreter ref read"
        written <- runViaCLI repo $ do
          h <- writeBlob content
          tree <- writeTree [BlobEntry "f" h]
          c <- writeCommit CommitData { commitParents = [], commitTree = tree, commitMessage = "msg" }
          createRef (RefName "refs/heads/story/ffi-diff") c
          return c
        resolvedFFI <- runViaFFI repo (resolveRef (RefName "refs/heads/story/ffi-diff"))
        resolvedFFI `shouldBe` Just written

    it "both round-trip a commit whose tree is read back correctly" $
      withSystemTempDirectory "gitlib-effect-ffi-diff" $ \parent -> do
        (cli, ffi) <- runBoth parent $ do
          h1   <- writeBlob "atom content"
          tree <- writeTree [BlobEntry "atom.md" h1]
          h2   <- writeCommit CommitData
                    { commitParents = [], commitTree = tree, commitMessage = "first tick" }
          cd   <- readCommit h2
          return (h2, tree, cd)
        ffi `shouldBe` cli

    it "both follow a ref through a commit to its self-hashed parent identically" $
      withSystemTempDirectory "gitlib-effect-ffi-diff" $ \parent -> do
        (cli, ffi) <- runBoth parent $ do
          h1    <- writeBlob "root"
          tree1 <- writeTree [BlobEntry "f" h1]
          root  <- writeCommit CommitData { commitParents = [], commitTree = tree1, commitMessage = "root" }
          h2    <- writeBlob "rootchild"
          tree2 <- writeTree [BlobEntry "f" h2]
          child <- writeCommit CommitData
                     { commitParents = [root], commitTree = tree2, commitMessage = "child" }
          createRef (RefName "refs/heads/story/chain") child
          resolved <- resolveRef (RefName "refs/heads/story/chain")
          cd <- readCommit child
          return (resolved, commitParents cd, root)
        ffi `shouldBe` cli

    it "both compute isAncestorOfAny identically over the same commit graph" $
      withSystemTempDirectory "gitlib-effect-ffi-diff" $ \parent -> do
        (cli, ffi) <- runBoth parent $ do
          h1   <- writeBlob "root"
          tree <- writeTree [BlobEntry "f" h1]
          root <- writeCommit CommitData { commitParents = [], commitTree = tree, commitMessage = "root" }
          mid  <- writeCommit CommitData { commitParents = [root], commitTree = tree, commitMessage = "mid" }
          tip  <- writeCommit CommitData { commitParents = [mid], commitTree = tree, commitMessage = "tip" }
          h2        <- writeBlob "other root"
          tree2     <- writeTree [BlobEntry "g" h2]
          unrelated <- writeCommit CommitData
                         { commitParents = [], commitTree = tree2, commitMessage = "unrelated" }
          hitsRoot        <- isAncestorOfAny [root] tip
          hitsMid         <- isAncestorOfAny [mid] tip
          hitsSelf        <- isAncestorOfAny [tip] tip
          missesUnrelated <- isAncestorOfAny [unrelated] tip
          return (hitsRoot, hitsMid, hitsSelf, missesUnrelated)
        ffi `shouldBe` cli
        ffi `shouldBe` (True, True, True, False)

    it "both resolve lookupPath through a nested tree identically" $
      withSystemTempDirectory "gitlib-effect-ffi-diff" $ \parent -> do
        (cli, ffi) <- runBoth parent $ do
          h1       <- writeBlob "chapter one"
          h2       <- writeBlob "readme"
          subtree  <- writeTree [BlobEntry "ch1.md" h1]
          rootTree <- writeTree [SubTree "chapters" subtree, BlobEntry "README.md" h2]
          found    <- lookupPath rootTree "chapters/ch1.md"
          missing  <- lookupPath rootTree "chapters/does-not-exist.md"
          return (found, missing, h1)
        ffi `shouldBe` cli
        let (found, missing, h1) = ffi
        found `shouldBe` Just h1
        missing `shouldBe` Nothing

    it "both list refs under a prefix identically" $
      withSystemTempDirectory "gitlib-effect-ffi-diff" $ \parent -> do
        (cli, ffi) <- runBoth parent $ do
          h    <- writeBlob "content"
          tree <- writeTree [BlobEntry "f" h]
          c1   <- writeCommit CommitData { commitParents = [], commitTree = tree, commitMessage = "a" }
          c2   <- writeCommit CommitData { commitParents = [], commitTree = tree, commitMessage = "b" }
          createRef (RefName "refs/heads/story/one") c1
          createRef (RefName "refs/heads/story/two") c2
          createRef (RefName "refs/heads/other/three") c1
          refs <- listRefs "refs/heads/story/"
          return (Set.fromList refs, c1, c2)
        ffi `shouldBe` cli
        let (refs, c1, c2) = ffi
        refs `shouldBe` Set.fromList
          [ (RefName "refs/heads/story/one", c1), (RefName "refs/heads/story/two", c2) ]

    it "both make a deleted ref unresolvable afterward" $
      withSystemTempDirectory "gitlib-effect-ffi-diff" $ \parent -> do
        (cli, ffi) <- runBoth parent $ do
          h    <- writeBlob "content"
          tree <- writeTree [BlobEntry "f" h]
          c    <- writeCommit CommitData { commitParents = [], commitTree = tree, commitMessage = "a" }
          createRef (RefName "refs/heads/story/gone") c
          resolvedBefore <- resolveRef (RefName "refs/heads/story/gone")
          deleteRef (RefName "refs/heads/story/gone")
          resolvedAfter <- resolveRef (RefName "refs/heads/story/gone")
          return (resolvedBefore, resolvedAfter, c)
        ffi `shouldBe` cli
        let (resolvedBefore, resolvedAfter, c) = ffi
        resolvedBefore `shouldBe` Just c
        resolvedAfter `shouldBe` Nothing

    -- 'Storyteller.Core.Git.rewriteChain' never reuses the original commit
    -- bytes for a rewrite -- it always goes read-then-reconstruct
    -- (@RG.readCommit hash@, then @RG.writeCommit cd { commitParents =
    -- newParents }@ using the *parsed-back* 'CommitData', not the raw
    -- bytes originally written). If 'readCommit' parses a multi-line
    -- message, embedded blank lines, or trailing whitespace even slightly
    -- differently between interpreters, a read-then-rewrite round trip is
    -- exactly where that would surface as a hash mismatch -- silently
    -- defeating 'withGitCache' from that point on, since every subsequent
    -- reference to the "same" commit would now disagree on its hash
    -- between the two interpreters. A direct compare of 'writeCommit's
    -- return value (not just the read-back 'CommitData') on a message
    -- with the kind of edge cases real content has is the point of this
    -- test, not the simpler "one-line message" cases above.
    it "both produce the same hash for a read-then-rewrite of a multi-line-message commit" $
      withSystemTempDirectory "gitlib-effect-ffi-diff" $ \parent -> do
        (cli, ffi) <- runBoth parent $ do
          h1     <- writeBlob "root"
          tree   <- writeTree [BlobEntry "f" h1]
          root   <- writeCommit CommitData
                      { commitParents = []
                      , commitTree    = tree
                      , commitMessage = "root"
                      }
          let msg = "line one\n\nline three, blank above\ntrailing spaces   \n\xe2\x9c\x93 unicode check"
          original <- writeCommit CommitData
                        { commitParents = [root]
                        , commitTree    = tree
                        , commitMessage = msg
                        }
          -- Simulate 'rewriteChain' exactly: read it back, then rewrite
          -- using the *parsed* CommitData with a genuinely different
          -- parent (forcing the rewrite path, not the "nothing changed"
          -- short-circuit) -- not the original 'msg'/'tree' values above.
          cd        <- readCommit original
          otherRoot <- writeCommit CommitData
                         { commitParents = [], commitTree = tree, commitMessage = "other root" }
          rewritten <- writeCommit cd { commitParents = [otherRoot] }
          rewrittenMessage <- commitMessage <$> readCommit rewritten
          return (original, rewritten, rewrittenMessage)
        ffi `shouldBe` cli
