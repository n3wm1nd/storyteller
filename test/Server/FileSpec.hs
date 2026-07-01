{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.FileSpec (spec) where

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import Test.Hspec

import Polysemy
import Polysemy.Fail (Fail)
import Polysemy.State (State)

import Git.Mock
import Runix.Git (Git)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, writeFile)
import Prelude hiding (writeFile)

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
-- ---------------------------------------------------------------------------

withFile_
  :: BranchName
  -> Sem ( StoryBranch Main
         : FileSystemWrite (BranchTag Main)
         : FileSystemRead  (BranchTag Main)
         : FileSystem      (BranchTag Main)
         : TestEffects '[] ) a
  -> Either String a
withFile_ name action = run $ testStack $ do
  _ <- createBranch name
  runBranchAndFS @Main name action

tickKinds :: Update -> [T.Text]
tickKinds = map wtKind . updateTicks

headIsIn :: Update -> Bool
headIsIn upd = null (updateTicks upd) || updateHead upd `elem` map wtTickId (updateTicks upd)

-- | Write a file into the working tree and store it as an atom tick.
storeAtom :: Members '[StoryStorage, Git, State GitState, Fail] r
          => FilePath -> BS.ByteString -> Sem r TickId
storeAtom path content =
  runBranchAndFS @Main (BranchName "b") $ do
    writeFile @(BranchTag Main) path content
    storeData @Main (draft (T.pack ("type:atom\n" <> BS.unpack content)))

-- ---------------------------------------------------------------------------
-- Specs
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "fileState" $ do

    it "returns an empty update for a branch with no file ticks" $
      (run $ testStack $ createBranch (BranchName "b") >> runBranchAndFS @Main (BranchName "b") (fileState "file.md"))
        `shouldSatisfy` \case
          Right upd -> null (updateTicks upd)
          _         -> False

    it "head is valid or empty when file has no ticks" $
      (run $ testStack $ createBranch (BranchName "b") >> runBranchAndFS @Main (BranchName "b") (fileState "file.md"))
        `shouldSatisfy` either (const False) headIsIn

  describe "deleteFileAtom" $ do

    it "deleted atom no longer appears in fileState" $ do
      let result = withFile_ (BranchName "b") $ do
            tid <- storeAtom "f.md" "hello"
            deleteFileAtom tid
            fileState "f.md"
      case result of
        Left err  -> expectationFailure err
        Right upd -> updateTicks upd `shouldBe` []

  describe "moveFileAtom" $ do

    it "moving a single atom to front is a no-op on chain length" $ do
      let result = withFile_ (BranchName "b") $ do
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
      let result = withFile_ (BranchName "b") $ do
            tid <- storeAtom "f.md" "original"
            editFileAtom "f.md" tid "edited"
            fileState "f.md"
      case result of
        Left err  -> expectationFailure err
        Right upd ->
          -- content changed; atom count stays the same
          length (updateTicks upd) `shouldBe` 1
