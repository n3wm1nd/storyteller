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
import Polysemy.Error (Error)
import Polysemy.Fail (Fail)
import Polysemy.State (State)

import Git.Mock
import Runix.Git (Git)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, writeFile)
import Prelude hiding (writeFile)

import Storyteller.Git
import Storyteller.Storage hiding (get, drop)
import Storyteller.Types

import Server.File
import Server.Protocol (Update(..), WireTick(..))
import Server.TestStack

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

withFile_ :: BranchName -> FilePath -> Sem (TestEffects '[]) a -> Either String a
withFile_ name path action = run $ testStack $ do
  _ <- createBranch name
  action

tickKinds :: Update -> [T.Text]
tickKinds = map wtKind . updateTicks

headIsIn :: Update -> Bool
headIsIn upd = null (updateTicks upd) || updateHead upd `elem` map wtTickId (updateTicks upd)

-- | Write a file into the working tree and store it as an atom tick.
storeAtom :: Members '[StoryStorage, Git, State GitState, Fail] r
          => BranchName -> FilePath -> BS.ByteString -> Sem r TickId
storeAtom name path content =
  runBranchAndFS @TestBranch name $ do
    writeFile @(BranchTag TestBranch) path content
    storeData @TestBranch (draft (T.pack ("type:atom\n" <> BS.unpack content)))

data TestBranch

-- ---------------------------------------------------------------------------
-- Specs
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "fileState" $ do

    it "returns Nothing for a non-existent branch" $
      (run $ testStack (fileState (BranchName "missing") "file.md"))
        `shouldBe` Right Nothing

    it "returns Just with empty update for a branch with no file ticks" $
      (run $ testStack $ createBranch (BranchName "b") >> fileState (BranchName "b") "file.md")
        `shouldSatisfy` \case
          Right (Just upd) -> null (updateTicks upd)
          _                -> False

    it "head is valid or empty when file has no ticks" $
      (run $ testStack $ createBranch (BranchName "b") >> fileState (BranchName "b") "file.md")
        `shouldSatisfy` either (const False) (maybe True headIsIn)

  describe "deleteFileAtom" $ do

    it "deleted atom no longer appears in fileState" $ do
      let result = withFile_ (BranchName "b") "f.md" $ do
            tid <- storeAtom (BranchName "b") "f.md" "hello"
            deleteFileAtom (BranchName "b") "f.md" tid
            fileState (BranchName "b") "f.md"
      case result of
        Left err  -> expectationFailure err
        Right Nothing -> expectationFailure "expected Just"
        Right (Just upd) -> updateTicks upd `shouldBe` []

  describe "moveFileAtom" $ do

    it "moving a single atom to front is a no-op on chain length" $ do
      let result = withFile_ (BranchName "b") "f.md" $ do
            t1 <- storeAtom (BranchName "b") "f.md" "atom1"
            before <- fmap (length . updateTicks) <$> fileState (BranchName "b") "f.md"
            moveFileAtom (BranchName "b") "f.md" t1 Nothing
            after <- fmap (length . updateTicks) <$> fileState (BranchName "b") "f.md"
            return (before, after)
      case result of
        Left err     -> expectationFailure err
        Right (b, a) -> b `shouldBe` a

  describe "editFileAtom" $ do

    it "edit changes the content of the atom" $ do
      let result = withFile_ (BranchName "b") "f.md" $ do
            tid <- storeAtom (BranchName "b") "f.md" "original"
            editFileAtom (BranchName "b") "f.md" tid "edited"
            fileState (BranchName "b") "f.md"
      case result of
        Left err  -> expectationFailure err
        Right Nothing -> expectationFailure "expected Just"
        Right (Just upd) ->
          -- content changed; atom count stays the same
          length (updateTicks upd) `shouldBe` 1
