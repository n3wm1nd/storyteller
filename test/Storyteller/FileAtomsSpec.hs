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
import Storyteller.Agent (Prompt(..))

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
-- Helpers: extract only atoms (ticks with Just content) for backward-compat tests
-- ---------------------------------------------------------------------------

atomsOnly :: [S.FileTick] -> [S.FileTick]
atomsOnly = filter (\ft -> S.ftKind ft == "atom")

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "fileTicks" $ do

  it "returns empty list when file has never existed" $ do
    let result = runTestFS $ fileTicks @Main "scene.md"
    result `shouldBe` Right []

  it "returns one atom tick after a single append" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "hello\n"
          _ <- store @Main "first"
          fileTicks @Main "scene.md"
    case result of
      Left err     -> fail err
      Right ticks  ->
        case atomsOnly ticks of
          [atom] -> S.ftContent atom `shouldBe` Just "hello\n"
          atoms  -> fail $ "expected 1 atom, got " <> show (length atoms)

  it "returns atoms oldest-first with correct content splits" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "p1\n"
          _ <- store @Main "first"
          appendFile @Main "scene.md" "p2\n"
          _ <- store @Main "second"
          appendFile @Main "scene.md" "p3\n"
          _ <- store @Main "third"
          fileTicks @Main "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> do
        let atoms = atomsOnly ticks
        length atoms `shouldBe` 3
        map S.ftContent atoms `shouldBe` [Just "p1\n", Just "p2\n", Just "p3\n"]
        -- each atom's parent is the preceding atom's tickId
        S.ftParent (atoms !! 1) `shouldBe` Just (S.ftTickId (atoms !! 0))
        S.ftParent (atoms !! 2) `shouldBe` Just (S.ftTickId (atoms !! 1))

  it "tick IDs match chain order: each parent is the previous atom's tickId" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "a\n"
          t1 <- store @Main "t1"
          appendFile @Main "scene.md" "b\n"
          t2 <- store @Main "t2"
          appendFile @Main "scene.md" "c\n"
          t3 <- store @Main "t3"
          ticks <- fileTicks @Main "scene.md"
          return (ticks, [t1, t2, t3])
    case result of
      Left err -> fail err
      Right (ticks, [t1, t2, t3]) -> do
        let atoms = atomsOnly ticks
        map S.ftTickId atoms `shouldBe` map unTickId [t1, t2, t3]
      Right _ -> fail "unexpected pattern"

  it "commits that don't touch the file are not included" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "a\n"
          _ <- store @Main "scene tick"
          appendFile @Main "notes.md" "note\n"
          _ <- store @Main "notes tick"
          appendFile @Main "scene.md" "b\n"
          _ <- store @Main "scene tick 2"
          fileTicks @Main "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> do
        let atoms = atomsOnly ticks
        length atoms `shouldBe` 2
        map S.ftContent atoms `shouldBe` [Just "a\n", Just "b\n"]

  it "after file deletion and re-creation, only post-creation atoms appear" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "before\n"
          _ <- store @Main "add"
          remove @(BranchTag Main) False "scene.md"
          _ <- store @Main "delete"
          appendFile @Main "scene.md" "after\n"
          _ <- store @Main "recreate"
          fileTicks @Main "scene.md"
    case result of
      Left err    -> fail err
      Right ticks ->
        case atomsOnly ticks of
          [a0, a1] -> do
            S.ftContent a0 `shouldBe` Just "before\n"
            S.ftContent a1 `shouldBe` Just "after\n"
          atoms -> fail $ "expected 2 atoms, got " <> show (length atoms)

  it "concatenating all atom contents reconstructs the full file" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "line one\n"
          _ <- store @Main "t1"
          appendFile @Main "scene.md" "line two\n"
          _ <- store @Main "t2"
          appendFile @Main "scene.md" "line three\n"
          _ <- store @Main "t3"
          ticks   <- fileTicks @Main "scene.md"
          current <- readFile @(BranchTag Main) "scene.md"
          return (ticks, current)
    case result of
      Left err -> fail err
      Right (ticks, current) ->
        let atoms = atomsOnly ticks
            contents = [ c | Just c <- map S.ftContent atoms ]
        in BS.concat (map (BS.pack . T.unpack) contents) `shouldBe` current

  it "message field carries the commit message" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "x\n"
          _ <- store @Main "my commit message"
          fileTicks @Main "scene.md"
    case result of
      Left err    -> fail err
      Right ticks ->
        case atomsOnly ticks of
          [atom] -> S.ftMessage atom `shouldBe` "my commit message"
          atoms  -> fail $ "expected 1 atom, got " <> show (length atoms)

  it "note referencing a file atom is included in that file's ticks" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "content\n"
          atomId <- store @Main "atom"
          noteId <- storeAs @Main (Note [atomId] "a note")
          fileTicks @Main "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> do
        let kinds = map S.ftKind ticks
        kinds `shouldContain` ["atom"]
        kinds `shouldContain` ["note"]
        -- the note's refs should point to the atom
        case filter (\ft -> S.ftKind ft == "note") ticks of
          [note] -> S.ftRefs note `shouldBe` [S.ftTickId (head (atomsOnly ticks))]
          notes  -> fail $ "expected 1 note, got " <> show (length notes)

  it "note referencing an atom in a different file is not included" $ do
    let result = runTestFS $ do
          appendFile @Main "other.md" "other content\n"
          atomId <- store @Main "other atom"
          _noteId <- storeAs @Main (Note [atomId] "note about other file")
          appendFile @Main "scene.md" "scene content\n"
          _ <- store @Main "scene atom"
          fileTicks @Main "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> do
        -- only the scene.md atom, no note (it refs other.md's atom)
        map S.ftKind ticks `shouldBe` ["atom"]

  it "note referencing a note is included transitively" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "content\n"
          atomId  <- store @Main "atom"
          noteId  <- storeAs @Main (Note [atomId] "first note")
          _note2Id <- storeAs @Main (Note [noteId] "note about note")
          fileTicks @Main "scene.md"
    case result of
      Left err    -> fail err
      Right ticks ->
        -- both notes are included: first refs atom, second refs first note
        length (filter (\ft -> S.ftKind ft == "note") ticks) `shouldBe` 2

  it "prompt with matching file field is included" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "content\n"
          _ <- store @Main "atom"
          _ <- storeAs @Main (Prompt "scene.md" "write more")
          fileTicks @Main "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> map S.ftKind ticks `shouldContain` ["prompt"]

  it "prompt for a different file is not included" $ do
    let result = runTestFS $ do
          appendFile @Main "scene.md" "content\n"
          _ <- store @Main "atom"
          _ <- storeAs @Main (Prompt "other.md" "write more")
          fileTicks @Main "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> map S.ftKind ticks `shouldBe` ["atom"]
