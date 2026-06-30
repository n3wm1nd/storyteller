{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Storyteller.FileAtomsSpec (spec) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State

import Runix.Git (Git)
import Runix.FileSystem
  ( FileSystem, FileSystemRead, FileSystemWrite
  , writeFile, readFile, remove )
import Prelude hiding (appendFile, readFile, writeFile)

import Git.Mock

import Storyteller.Types
import Storyteller.Storage hiding (get, drop)
import qualified Storyteller.Storage as S
import Storyteller.Git

-- ---------------------------------------------------------------------------
-- Helpers & runner
-- ---------------------------------------------------------------------------

data Main

appendFile :: forall branch r. Members '[FileSystemRead (BranchTag branch), FileSystemWrite (BranchTag branch), Fail] r
           => FilePath -> BS.ByteString -> Sem r ()
appendFile path content = do
  existing <- runFail $ readFile @(BranchTag branch) path
  let base = either (const BS.empty) id existing
  writeFile @(BranchTag branch) path (base <> content)

runTestFS
  :: Sem '[ StoryBranch Main
          , FileSystemWrite (BranchTag Main)
          , FileSystemRead  (BranchTag Main)
          , FileSystem      (BranchTag Main)
          , StoryStorage
          , Git
          , State WorkingTree
          , State GitState
          , Fail
          ] a
  -> Either String a
runTestFS action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . evalState emptyWorkingTree
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "main")
      runStoryFSGit @Main (BranchName "main")
        . runStoryBranchGit @Main (BranchName "main")
        . subsume_
        $ action

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "fileAtoms" $ do

  it "returns empty list when file has never existed" $ do
    let result = runTestFS $ fileAtoms @Main "scene.md"
    result `shouldBe` Right []

  it "returns one atom after a single append" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "hello\n"
          _ <- store @Main "first"
          fileAtoms @Main "scene.md"
    case result of
      Left err         -> fail err
      Right [atom]     -> aeContent atom `shouldBe` "hello\n"
      Right atoms      -> fail $ "expected 1 atom, got " <> show (length atoms)

  it "returns atoms oldest-first with correct content splits" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "p1\n"
          _ <- store @Main "first"
          appendFile @Main "scene.md" "p2\n"
          _ <- store @Main "second"
          appendFile @Main "scene.md" "p3\n"
          _ <- store @Main "third"
          fileAtoms @Main "scene.md"
    case result of
      Left err    -> fail err
      Right atoms -> do
        length atoms `shouldBe` 3
        map aeContent atoms `shouldBe` ["p1\n", "p2\n", "p3\n"]
        -- each atom's parent is the preceding atom's tickId
        aeParent (atoms !! 1) `shouldBe` Just (aeTickId (atoms !! 0))
        aeParent (atoms !! 2) `shouldBe` Just (aeTickId (atoms !! 1))

  it "tick IDs match chain order: each parent is the previous atom's tickId" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "a\n"
          t1 <- store @Main "t1"
          appendFile @Main "scene.md" "b\n"
          t2 <- store @Main "t2"
          appendFile @Main "scene.md" "c\n"
          t3 <- store @Main "t3"
          atoms <- fileAtoms @Main "scene.md"
          return (atoms, [t1, t2, t3])
    case result of
      Left err -> fail err
      Right (atoms, [t1, t2, t3]) -> do
        map (aeTickId) atoms `shouldBe` map unTickId [t1, t2, t3]
      Right _ -> fail "unexpected pattern"

  it "commits that don't touch the file are not included" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "a\n"
          _ <- store @Main "scene tick"
          appendFile @Main "notes.md" "note\n"
          _ <- store @Main "notes tick"
          appendFile @Main "scene.md" "b\n"
          _ <- store @Main "scene tick 2"
          fileAtoms @Main "scene.md"
    case result of
      Left err    -> fail err
      Right atoms -> do
        length atoms `shouldBe` 2
        map aeContent atoms `shouldBe` ["a\n", "b\n"]

  it "after file deletion and re-creation, only post-creation atoms appear" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "before\n"
          _ <- store @Main "add"
          remove @(BranchTag Main) False "scene.md"
          _ <- store @Main "delete"
          appendFile @Main "scene.md" "after\n"
          _ <- store @Main "recreate"
          fileAtoms @Main "scene.md"
    case result of
      Left err    -> fail err
      Right [a0, a1] -> do
        aeContent a0 `shouldBe` "before\n"
        aeContent a1 `shouldBe` "after\n"
      Right atoms -> fail $ "expected 2 atoms, got " <> show (length atoms)

  it "concatenating all atom contents reconstructs the full file" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "line one\n"
          _ <- store @Main "t1"
          appendFile @Main "scene.md" "line two\n"
          _ <- store @Main "t2"
          appendFile @Main "scene.md" "line three\n"
          _ <- store @Main "t3"
          atoms   <- fileAtoms @Main "scene.md"
          current <- readFile @(BranchTag Main) "scene.md"
          return (atoms, current)
    case result of
      Left err -> fail err
      Right (atoms, current) ->
        BS.concat (map (BS.pack . T.unpack . aeContent) atoms) `shouldBe` current

  it "message field carries the commit message" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "x\n"
          _ <- store @Main "my commit message"
          fileAtoms @Main "scene.md"
    case result of
      Left err    -> fail err
      Right [atom] -> aeMessage atom `shouldBe` "my commit message"
      Right atoms  -> fail $ "expected 1 atom, got " <> show (length atoms)
