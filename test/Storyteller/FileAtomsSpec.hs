{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Storyteller.FileAtomsSpec (spec) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State
import Runix.Git (Git, ObjectHash(..))

import Git.Mock

import Storyteller.Core.Types
import Storyteller.Common.Types (Note(..))
import Storyteller.Core.Storage (createBranch)
import qualified Storyteller.Core.StorageMonad as SM
import Storyteller.Core.Git (runStoryStorageGit)
import Storyteller.Writer.Agent (Prompt(..))

-- ---------------------------------------------------------------------------
-- Helpers & runner
-- ---------------------------------------------------------------------------

runTestFS
  :: (forall n. SM.StorageM n => SM.StorageT n a)
  -> Either String a
runTestFS action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  $ do
      b <- createBranch (BranchName "main")
      let headHash0 = ObjectHash (unTickId (branchHead b))
      wt0 <- SM.loadWorkingTree headHash0
      fst <$> SM.runStorageT headHash0 wt0 action

-- ---------------------------------------------------------------------------
-- Helpers: extract only atoms (ticks with Just content) for backward-compat tests
-- ---------------------------------------------------------------------------

atomsOnly :: [SM.FileTick] -> [SM.FileTick]
atomsOnly = filter (\ft -> SM.ftKind ft == "atom")

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "fileTicks" $ do

  it "returns empty list when file has never existed" $ do
    let result = runTestFS $ SM.fileTicksOf "scene.md"
    result `shouldBe` Right []

  it "returns one atom tick after a single append" $ do
    let result = runTestFS $ do
          _ <- SM.appendAtom "scene.md" "hello\n"
          SM.fileTicksOf "scene.md"
    case result of
      Left err     -> fail err
      Right ticks  ->
        case atomsOnly ticks of
          [atom] -> SM.ftContent atom `shouldBe` Just "hello\n"
          atoms  -> fail $ "expected 1 atom, got " <> show (length atoms)

  it "returns atoms oldest-first with correct content splits" $ do
    let result = runTestFS $ do
          _ <- SM.appendAtom "scene.md" "p1\n"
          _ <- SM.appendAtom "scene.md" "p2\n"
          _ <- SM.appendAtom "scene.md" "p3\n"
          SM.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> do
        let atoms = atomsOnly ticks
        length atoms `shouldBe` 3
        map SM.ftContent atoms `shouldBe` [Just "p1\n", Just "p2\n", Just "p3\n"]
        -- each atom's parent is the preceding atom's tickId
        SM.ftParent (atoms !! 1) `shouldBe` Just (SM.ftTickId (atoms !! 0))
        SM.ftParent (atoms !! 2) `shouldBe` Just (SM.ftTickId (atoms !! 1))

  it "tick IDs match chain order: each parent is the previous atom's tickId" $ do
    let result = runTestFS $ do
          t1 <- SM.appendAtom "scene.md" "a\n"
          t2 <- SM.appendAtom "scene.md" "b\n"
          t3 <- SM.appendAtom "scene.md" "c\n"
          ticks <- SM.fileTicksOf "scene.md"
          return (ticks, [t1, t2, t3])
    case result of
      Left err -> fail err
      Right (ticks, [t1, t2, t3]) -> do
        let atoms = atomsOnly ticks
        map SM.ftTickId atoms `shouldBe` map unTickId [t1, t2, t3]
      Right _ -> fail "unexpected pattern"

  it "commits that don't touch the file are not included" $ do
    let result = runTestFS $ do
          _ <- SM.appendAtom "scene.md" "a\n"
          _ <- SM.appendAtom "notes.md" "note\n"
          _ <- SM.appendAtom "scene.md" "b\n"
          SM.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> do
        let atoms = atomsOnly ticks
        length atoms `shouldBe` 2
        map SM.ftContent atoms `shouldBe` [Just "a\n", Just "b\n"]

  it "after file deletion and re-creation, only post-creation atoms appear" $ do
    let result = runTestFS $ do
          _ <- SM.appendAtom "scene.md" "before\n"
          SM.removeS False "scene.md"
          _ <- SM.store (draft "delete")
          _ <- SM.appendAtom "scene.md" "after\n"
          SM.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks ->
        case atomsOnly ticks of
          [a0, a1] -> do
            SM.ftContent a0 `shouldBe` Just "before\n"
            SM.ftContent a1 `shouldBe` Just "after\n"
          atoms -> fail $ "expected 2 atoms, got " <> show (length atoms)

  it "concatenating all atom contents reconstructs the full file" $ do
    let result = runTestFS $ do
          _ <- SM.appendAtom "scene.md" "line one\n"
          _ <- SM.appendAtom "scene.md" "line two\n"
          _ <- SM.appendAtom "scene.md" "line three\n"
          ticks   <- SM.fileTicksOf "scene.md"
          current <- SM.readFileS "scene.md"
          return (ticks, current)
    case result of
      Left err -> fail err
      Right (ticks, current) ->
        let atoms = atomsOnly ticks
            contents = [ c | Just c <- map SM.ftContent atoms ]
        in BS.concat (map (BS.pack . T.unpack) contents) `shouldBe` current

  it "message field carries the atom's own content (an atom's message *is* its content)" $ do
    let result = runTestFS $ do
          _ <- SM.appendAtom "scene.md" "x\n"
          SM.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks ->
        case atomsOnly ticks of
          [atom] -> SM.ftMessage atom `shouldBe` "x\n"
          atoms  -> fail $ "expected 1 atom, got " <> show (length atoms)

  it "note referencing a file atom is included in that file's ticks" $ do
    let result = runTestFS $ do
          atomId <- SM.appendAtom "scene.md" "content\n"
          _noteId <- SM.storeAs (Note [atomId] "a note")
          SM.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> do
        let kinds = map SM.ftKind ticks
        kinds `shouldContain` ["atom"]
        kinds `shouldContain` ["note"]
        -- the note's refs should point to the atom
        case filter (\ft -> SM.ftKind ft == "note") ticks of
          [note] -> SM.ftRefs note `shouldBe` [SM.ftTickId (head (atomsOnly ticks))]
          notes  -> fail $ "expected 1 note, got " <> show (length notes)

  it "note referencing an atom in a different file is not included" $ do
    let result = runTestFS $ do
          atomId <- SM.appendAtom "other.md" "other content\n"
          _noteId <- SM.storeAs (Note [atomId] "note about other file")
          _ <- SM.appendAtom "scene.md" "scene content\n"
          SM.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> do
        -- only the scene.md atom, no note (it refs other.md's atom)
        map SM.ftKind ticks `shouldBe` ["atom"]

  it "note referencing a note is included transitively" $ do
    let result = runTestFS $ do
          atomId  <- SM.appendAtom "scene.md" "content\n"
          noteId  <- SM.storeAs (Note [atomId] "first note")
          _note2Id <- SM.storeAs (Note [noteId] "note about note")
          SM.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks ->
        -- both notes are included: first refs atom, second refs first note
        length (filter (\ft -> SM.ftKind ft == "note") ticks) `shouldBe` 2

  it "prompt with matching file field is included" $ do
    let result = runTestFS $ do
          _ <- SM.appendAtom "scene.md" "content\n"
          _ <- SM.storeAs (Prompt "scene.md" "write more")
          SM.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> map SM.ftKind ticks `shouldContain` ["prompt"]

  it "prompt for a different file is not included" $ do
    let result = runTestFS $ do
          _ <- SM.appendAtom "scene.md" "content\n"
          _ <- SM.storeAs (Prompt "other.md" "write more")
          SM.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> map SM.ftKind ticks `shouldBe` ["atom"]
