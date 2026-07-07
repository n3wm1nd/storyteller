{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 'Storyteller.Core.Create.createFile' introduces a path into the tree as
-- its own tick, with empty content — distinct from the first real 'Atom' a
-- 'Storage.Ops.append' would otherwise land implicitly. Pins:
--
--   * the tick this produces is an ordinary, empty atom (not a distinct
--     "created" kind — see 'Storyteller.Core.Create's own doc for why);
--   * its content is empty, not absent;
--   * content appended afterward lands as its own, separate atom tick.
module Storyteller.CreateSpec (spec) where

import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState)

import Git.Mock

import Storyteller.Core.Git (runStoryStorageGit)
import Storyteller.Core.Storage (createBranch)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Core.Types
import Storyteller.Core.Create (createFile)

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

spec :: Spec
spec = describe "createFile" $ do

  it "produces exactly one tick, an ordinary atom" $ do
    let result = runTestFS $ do
          _ <- createFile "scene.md"
          Tick.fileTicksOf "scene.md"
    case result of
      Left err     -> expectationFailure err
      Right ticks  -> map Tick.ftKind ticks `shouldBe` ["atom"]

  it "the introduction tick carries empty (not absent) content" $ do
    let result = runTestFS $ do
          _ <- createFile "scene.md"
          Tick.fileTicksOf "scene.md"
    case result of
      Left err    -> expectationFailure err
      Right [t]   -> Tick.ftContent t `shouldBe` Just ""
      Right ticks -> expectationFailure ("expected 1 tick, got " <> show (length ticks))

  it "content appended after creation lands as a separate atom tick" $ do
    let result = runTestFS $ do
          _ <- createFile "scene.md"
          _ <- Ops.append "scene.md" "hello\n"
          Tick.fileTicksOf "scene.md"
    case result of
      Left err              -> expectationFailure err
      Right [created, atom] -> do
        Tick.ftKind created `shouldBe` "atom"
        Tick.ftContent created `shouldBe` Just ""
        Tick.ftKind atom    `shouldBe` "atom"
        Tick.ftContent atom `shouldBe` Just "hello\n"
      Right ticks -> expectationFailure ("expected 2 ticks, got " <> show (length ticks))

  -- Regression: back when the introduction tick was its own distinct
  -- "created" tick kind, a chain-editing pop only special-cased 'Atom' --
  -- popping a "created" tick returned an empty file diff, silently losing
  -- the file's introduction entirely on replay. Moving the introduction
  -- tick elsewhere in the chain (which pops and re-pushes it) is the most
  -- direct way to exercise exactly that path.
  it "the introduction tick's file survives being moved elsewhere in the chain" $ do
    let result = runTestFS $ do
          tid0 <- createFile "scene.md"
          tid1 <- Ops.append "other.md" "unrelated\n"
          _    <- Ops.moveTick tid0 (Just tid1)
          exists  <- Ops.exists "scene.md"
          content <- if exists then Just <$> Core.readFile "scene.md" else return Nothing
          return (exists, content)
    case result of
      Left err -> expectationFailure err
      Right (exists, content) -> do
        exists `shouldBe` True
        content `shouldBe` Just ""
