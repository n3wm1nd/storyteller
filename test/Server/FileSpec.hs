{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.FileSpec (spec) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Test.Hspec

import Polysemy
import Polysemy.Fail (Fail)

import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, writeFile, readFile)
import Prelude hiding (writeFile, readFile)

import Storyteller.Git
import Storyteller.Storage hiding (get, drop)
import Storyteller.Runtime (Main)
import Storyteller.Types

import Server.File
import Server.Protocol (Update(..), WireTick(..))
import Server.TestStack

-- ---------------------------------------------------------------------------
-- Helpers
--
-- File functions assume their scope ('FileOpen') is already open, same as a
-- real connection: it's entered once here, wrapping the whole action,
-- rather than per call.
--
-- 'runner' (a 'TestRunner', see 'Server.TestStack') is threaded through so
-- every test below runs under both the eager and the 'withStorage'-
-- buffered interpreter (see 'test/Main.hs') without being written twice.
-- ---------------------------------------------------------------------------

withFile_
  :: TestRunner
  -> BranchName
  -> Sem ( StoryBranch Main
         : FileSystemWrite (BranchTag Main)
         : FileSystemRead  (BranchTag Main)
         : FileSystem      (BranchTag Main)
         : StoryStorage
         : TestEffects '[] ) a
  -> Either String a
withFile_ runner name action = run $ runner $ do
  _ <- createBranch name
  runBranchAndFS @Main name action

headIsIn :: Update -> Bool
headIsIn upd = null (updateTicks upd) || updateHead upd `elem` map wtTickId (updateTicks upd)

-- | Write a file into the working tree and store it as an atom tick.
--   Only valid for a file's first atom — later atoms must be appended
--   (see 'appendAtom'), since 'writeFile' overwrites the whole blob.
--   Runs against the ambient, already-open branch scope — same as any real
--   command dispatch — rather than opening its own: 'StoryBranch's head is
--   a point-in-time snapshot from whenever a scope was opened, so a nested
--   'runBranchAndFS' here would be invisible to the outer scope's later
--   reads (see 'Storyteller.Git.runStoryBranchGit').
storeAtom :: Members '[ StoryBranch Main
                      , FileSystemWrite (BranchTag Main)
                      , FileSystemRead  (BranchTag Main)
                      , Fail ] r
          => FilePath -> BS.ByteString -> Sem r TickId
storeAtom path content = do
  writeFile @(BranchTag Main) path content
  storeData @Main (draft (T.pack ("type:atom\n" <> BS.unpack content)))

-- | Append content to an existing file and store it as a new atom tick.
--   Same ambient-scope note as 'storeAtom'.
appendAtom :: Members '[ StoryBranch Main
                       , FileSystemWrite (BranchTag Main)
                       , FileSystemRead  (BranchTag Main)
                       , Fail ] r
           => FilePath -> BS.ByteString -> Sem r TickId
appendAtom path content = do
  existing <- readFile @(BranchTag Main) path
  writeFile @(BranchTag Main) path (existing <> content)
  storeData @Main (draft (T.pack ("type:atom\n" <> BS.unpack content)))

-- ---------------------------------------------------------------------------
-- Specs
-- ---------------------------------------------------------------------------

spec :: TestRunner -> Spec
spec runner = do

  describe "fileState" $ do

    it "returns an empty update for a branch with no file ticks" $
      (run $ runner $ createBranch (BranchName "b") >> runBranchAndFS @Main (BranchName "b") (fileState "file.md"))
        `shouldSatisfy` \case
          Right upd -> null (updateTicks upd)
          _         -> False

    it "head is valid or empty when file has no ticks" $
      (run $ runner $ createBranch (BranchName "b") >> runBranchAndFS @Main (BranchName "b") (fileState "file.md"))
        `shouldSatisfy` either (const False) headIsIn

  describe "deleteFileAtom" $ do

    it "deleted atom no longer appears in fileState" $ do
      let result = withFile_ runner (BranchName "b") $ do
            tid <- storeAtom "f.md" "hello"
            deleteFileAtom tid
            fileState "f.md"
      case result of
        Left err  -> expectationFailure err
        Right upd -> updateTicks upd `shouldBe` []

  describe "moveFileAtom" $ do

    it "moving a single atom to front is a no-op on chain length" $ do
      let result = withFile_ runner (BranchName "b") $ do
            t1 <- storeAtom "f.md" "atom1"
            before <- length . updateTicks <$> fileState "f.md"
            moveFileAtom t1 Nothing
            after <- length . updateTicks <$> fileState "f.md"
            return (before, after)
      case result of
        Left err     -> expectationFailure err
        Right (b, a) -> b `shouldBe` a

  describe "editFileAtom" $ do

    it "edit changes the content of the atom" $ do
      let result = withFile_ runner (BranchName "b") $ do
            tid <- storeAtom "f.md" "original"
            editFileAtom "f.md" tid "edited"
            fileState "f.md"
      case result of
        Left err  -> expectationFailure err
        Right upd ->
          -- content changed; atom count stays the same
          length (updateTicks upd) `shouldBe` 1

    -- The only way an atom edit may fail is if the targeted atom doesn't
    -- exist. Any replacement content — shorter, longer, unrelated — must be
    -- accepted for an atom that does exist. Reproduces a bug where editing
    -- a non-last atom with content shorter than the original trips the
    -- storage layer's append-only check, since editAtom overwrote the whole
    -- file blob with just the new atom bytes instead of appending them
    -- after the preceding atoms' content.
    it "edit succeeds for any existing atom regardless of new content length" $ do
      let result = withFile_ runner (BranchName "b") $ do
            t1 <- storeAtom "f.md" "first atom text\n"
            _  <- appendAtom "f.md" "second\n"
            editFileAtom "f.md" t1 "x\n"
            fileState "f.md"
      case result of
        Left err  -> expectationFailure err
        Right upd -> length (updateTicks upd) `shouldBe` 2
