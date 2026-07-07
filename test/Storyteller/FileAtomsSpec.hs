{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Storyteller.FileAtomsSpec (spec) where

import Prelude hiding (readFile)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State

import Git.Mock

import Storyteller.Core.Types
import Storyteller.Common.Types (Note(..))
import Storyteller.Core.Storage (createBranch)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Core.Git (runStoryStorageGit)
import Storyteller.Writer.Agent (Prompt(..))

-- ---------------------------------------------------------------------------
-- Helpers & runner
-- ---------------------------------------------------------------------------

runTestFS
  :: (forall n. Core.StoreM n => Core.StoreT n a)
  -> Either String a
runTestFS action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  $ do
      b <- createBranch (BranchName "main")
      let headHash0 = Core.ObjectHash (unTickId (branchHead b))
      fst <$> Core.runStoreT headHash0 action

-- ---------------------------------------------------------------------------
-- Helpers: extract only atoms (ticks with Just content) for backward-compat tests
-- ---------------------------------------------------------------------------

atomsOnly :: [Tick.FileTick] -> [Tick.FileTick]
atomsOnly = filter (\ft -> Tick.ftKind ft == "atom")

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "fileTicks" $ do

  it "returns empty list when file has never existed" $ do
    let result = runTestFS $ Tick.fileTicksOf "scene.md"
    result `shouldBe` Right []

  it "returns one atom tick after a single append" $ do
    let result = runTestFS $ do
          _ <- Ops.addAtom "scene.md" "hello\n"
          Tick.fileTicksOf "scene.md"
    case result of
      Left err     -> fail err
      Right ticks  ->
        case atomsOnly ticks of
          [atom] -> Tick.ftContent atom `shouldBe` Just "hello\n"
          atoms  -> fail $ "expected 1 atom, got " <> show (length atoms)

  it "returns atoms oldest-first with correct content splits" $ do
    let result = runTestFS $ do
          _ <- Ops.addAtom "scene.md" "p1\n"
          _ <- Ops.addAtom "scene.md" "p2\n"
          _ <- Ops.addAtom "scene.md" "p3\n"
          Tick.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> do
        let atoms = atomsOnly ticks
        length atoms `shouldBe` 3
        map Tick.ftContent atoms `shouldBe` [Just "p1\n", Just "p2\n", Just "p3\n"]
        -- each atom's parent is the preceding atom's tickId
        Tick.ftParent (atoms !! 1) `shouldBe` Just (Tick.ftTickId (atoms !! 0))
        Tick.ftParent (atoms !! 2) `shouldBe` Just (Tick.ftTickId (atoms !! 1))

  it "tick IDs match chain order: each parent is the previous atom's tickId" $ do
    let result = runTestFS $ do
          t1 <- Ops.addAtom "scene.md" "a\n"
          t2 <- Ops.addAtom "scene.md" "b\n"
          t3 <- Ops.addAtom "scene.md" "c\n"
          ticks <- Tick.fileTicksOf "scene.md"
          return (ticks, [t1, t2, t3])
    case result of
      Left err -> fail err
      Right (ticks, [t1, t2, t3]) -> do
        let atoms = atomsOnly ticks
        map Tick.ftTickId atoms `shouldBe` map Core.unObjectHash [t1, t2, t3]
      Right _ -> fail "unexpected pattern"

  it "commits that don't touch the file are not included" $ do
    let result = runTestFS $ do
          _ <- Ops.addAtom "scene.md" "a\n"
          _ <- Ops.addAtom "notes.md" "note\n"
          _ <- Ops.addAtom "scene.md" "b\n"
          Tick.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> do
        let atoms = atomsOnly ticks
        length atoms `shouldBe` 2
        map Tick.ftContent atoms `shouldBe` [Just "a\n", Just "b\n"]

  it "after file deletion and re-creation, only post-creation atoms appear" $ do
    let result = runTestFS $ do
          _ <- Ops.addAtom "scene.md" "before\n"
          Core.remove "scene.md"
          _ <- Core.store (Core.NonAtom [] "delete")
          _ <- Ops.addAtom "scene.md" "after\n"
          Tick.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks ->
        case atomsOnly ticks of
          [a0, a1] -> do
            Tick.ftContent a0 `shouldBe` Just "before\n"
            Tick.ftContent a1 `shouldBe` Just "after\n"
          atoms -> fail $ "expected 2 atoms, got " <> show (length atoms)

  it "concatenating all atom contents reconstructs the full file" $ do
    let result = runTestFS $ do
          _ <- Ops.addAtom "scene.md" "line one\n"
          _ <- Ops.addAtom "scene.md" "line two\n"
          _ <- Ops.addAtom "scene.md" "line three\n"
          ticks   <- Tick.fileTicksOf "scene.md"
          current <- Core.readFile "scene.md"
          return (ticks, current)
    case result of
      Left err -> fail err
      Right (ticks, current) ->
        let atoms = atomsOnly ticks
            contents = [ c | Just c <- map Tick.ftContent atoms ]
        in BS.concat (map (BS.pack . T.unpack) contents) `shouldBe` current

  it "message field carries the atom's own content (an atom's message *is* its content)" $ do
    let result = runTestFS $ do
          _ <- Ops.addAtom "scene.md" "x\n"
          Tick.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks ->
        case atomsOnly ticks of
          [atom] -> Tick.ftMessage atom `shouldBe` "x\n"
          atoms  -> fail $ "expected 1 atom, got " <> show (length atoms)

  it "note referencing a file atom is included in that file's ticks" $ do
    let result = runTestFS $ do
          atomId <- Ops.addAtom "scene.md" "content\n"
          _noteId <- Tick.storeAs (Note [TickId (Core.unObjectHash atomId)] "a note")
          Tick.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> do
        let kinds = map Tick.ftKind ticks
        kinds `shouldContain` ["atom"]
        kinds `shouldContain` ["note"]
        -- the note's refs should point to the atom
        case filter (\ft -> Tick.ftKind ft == "note") ticks of
          [note] -> Tick.ftRefs note `shouldBe` [Tick.ftTickId (head (atomsOnly ticks))]
          notes  -> fail $ "expected 1 note, got " <> show (length notes)

  it "note referencing an atom in a different file is not included" $ do
    let result = runTestFS $ do
          atomId <- Ops.addAtom "other.md" "other content\n"
          _noteId <- Tick.storeAs (Note [TickId (Core.unObjectHash atomId)] "note about other file")
          _ <- Ops.addAtom "scene.md" "scene content\n"
          Tick.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> do
        -- only the scene.md atom, no note (it refs other.md's atom)
        map Tick.ftKind ticks `shouldBe` ["atom"]

  it "note referencing a note is included transitively" $ do
    let result = runTestFS $ do
          atomId  <- Ops.addAtom "scene.md" "content\n"
          noteId  <- Tick.storeAs (Note [TickId (Core.unObjectHash atomId)] "first note")
          _note2Id <- Tick.storeAs (Note [TickId (Core.unObjectHash noteId)] "note about note")
          Tick.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks ->
        -- both notes are included: first refs atom, second refs first note
        length (filter (\ft -> Tick.ftKind ft == "note") ticks) `shouldBe` 2

  it "prompt with matching file field is included" $ do
    let result = runTestFS $ do
          _ <- Ops.addAtom "scene.md" "content\n"
          _ <- Tick.storeAs (Prompt "scene.md" "write more")
          Tick.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> map Tick.ftKind ticks `shouldContain` ["prompt"]

  it "prompt for a different file is not included" $ do
    let result = runTestFS $ do
          _ <- Ops.addAtom "scene.md" "content\n"
          _ <- Tick.storeAs (Prompt "other.md" "write more")
          Tick.fileTicksOf "scene.md"
    case result of
      Left err    -> fail err
      Right ticks -> map Tick.ftKind ticks `shouldBe` ["atom"]
