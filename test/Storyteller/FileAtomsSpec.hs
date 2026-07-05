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
  , readFile, remove )
import Prelude hiding (readFile)

import Git.Mock

import Storyteller.Core.Types
import Storyteller.Common.Types (Note(..))
import Storyteller.Core.Storage hiding (get, drop)
import qualified Storyteller.Core.Storage as S
import Storyteller.Core.Append (appendAtom)
import Storyteller.Core.Git
import Storyteller.Writer.Agent (Prompt(..))

-- ---------------------------------------------------------------------------
-- Helpers & runner
-- ---------------------------------------------------------------------------

data Main

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
          _ <- appendAtom @Main "scene.md" "hello\n"
          fileTicks @Main "scene.md"
    case result of
      Left err     -> fail err
      Right ticks  ->
        case atomsOnly ticks of
          [atom] -> S.ftContent atom `shouldBe` Just "hello\n"
          atoms  -> fail $ "expected 1 atom, got " <> show (length atoms)

  it "returns atoms oldest-first with correct content splits" $ do
    let result = runTestFS $ do
          _ <- appendAtom @Main "scene.md" "p1\n"
          _ <- appendAtom @Main "scene.md" "p2\n"
          _ <- appendAtom @Main "scene.md" "p3\n"
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
          t1 <- appendAtom @Main "scene.md" "a\n"
          t2 <- appendAtom @Main "scene.md" "b\n"
          t3 <- appendAtom @Main "scene.md" "c\n"
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
          _ <- appendAtom @Main "scene.md" "a\n"
          _ <- appendAtom @Main "notes.md" "note\n"
          _ <- appendAtom @Main "scene.md" "b\n"
          fileTicks @Main "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> do
        let atoms = atomsOnly ticks
        length atoms `shouldBe` 2
        map S.ftContent atoms `shouldBe` [Just "a\n", Just "b\n"]

  it "after file deletion and re-creation, only post-creation atoms appear" $ do
    let result = runTestFS $ do
          _ <- appendAtom @Main "scene.md" "before\n"
          remove @(BranchTag Main) False "scene.md"
          _ <- store @Main "delete"
          _ <- appendAtom @Main "scene.md" "after\n"
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
          _ <- appendAtom @Main "scene.md" "line one\n"
          _ <- appendAtom @Main "scene.md" "line two\n"
          _ <- appendAtom @Main "scene.md" "line three\n"
          ticks   <- fileTicks @Main "scene.md"
          current <- readFile @(BranchTag Main) "scene.md"
          return (ticks, current)
    case result of
      Left err -> fail err
      Right (ticks, current) ->
        let atoms = atomsOnly ticks
            contents = [ c | Just c <- map S.ftContent atoms ]
        in BS.concat (map (BS.pack . T.unpack) contents) `shouldBe` current

  it "message field carries the atom's own content (an atom's message *is* its content)" $ do
    let result = runTestFS $ do
          _ <- appendAtom @Main "scene.md" "x\n"
          fileTicks @Main "scene.md"
    case result of
      Left err    -> fail err
      Right ticks ->
        case atomsOnly ticks of
          [atom] -> S.ftMessage atom `shouldBe` "x\n"
          atoms  -> fail $ "expected 1 atom, got " <> show (length atoms)

  it "note referencing a file atom is included in that file's ticks" $ do
    let result = runTestFS $ do
          atomId <- appendAtom @Main "scene.md" "content\n"
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
          atomId <- appendAtom @Main "other.md" "other content\n"
          _noteId <- storeAs @Main (Note [atomId] "note about other file")
          _ <- appendAtom @Main "scene.md" "scene content\n"
          fileTicks @Main "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> do
        -- only the scene.md atom, no note (it refs other.md's atom)
        map S.ftKind ticks `shouldBe` ["atom"]

  it "note referencing a note is included transitively" $ do
    let result = runTestFS $ do
          atomId  <- appendAtom @Main "scene.md" "content\n"
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
          _ <- appendAtom @Main "scene.md" "content\n"
          _ <- storeAs @Main (Prompt "scene.md" "write more")
          fileTicks @Main "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> map S.ftKind ticks `shouldContain` ["prompt"]

  it "prompt for a different file is not included" $ do
    let result = runTestFS $ do
          _ <- appendAtom @Main "scene.md" "content\n"
          _ <- storeAs @Main (Prompt "other.md" "write more")
          fileTicks @Main "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> map S.ftKind ticks `shouldBe` ["atom"]
