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

  describe "Storage.Ops.deleteFile" $ do

    -- 'Opaque' is reserved for content this module never introduced itself
    -- (an external git commit, a hand-adopted repo) -- see 'Storage.Core's
    -- own Haddock on 'Storage.Core.Tick'. 'Storage.Ops.deleteFile' commits
    -- one ordinary, tagged 'Atom' -- it never goes near
    -- 'Storage.Ops.commitFile'/'commitWorktree's reconciliation at all, so
    -- there's no path to an 'Opaque' fallback here regardless of whether
    -- the file had content.
    it "never synthesizes an Opaque commit, whether the file had content or not" $ do
      let result = runTestFS $ do
            _ <- createFile "scene.md"
            _ <- Ops.append "scene.md" "hello\n"
            Ops.deleteFile "scene.md"
            Core.follow [] (\acc _ t -> (t : acc, True))
      case result of
        Left err    -> expectationFailure err
        Right ticks -> ticks `shouldSatisfy` all (not . isOpaque)

    -- Deletion is a forward event, not a rebase: the path disappears from
    -- the *tree*, but every earlier tick -- including the one that
    -- introduced it -- stays exactly where it was in history. Contrast
    -- with the old (wrong) rebase-based implementation, which made
    -- 'Tick.fileTicksOf' come back empty by physically excising those
    -- ticks from the chain.
    it "removes the path from the tree, but keeps its own tick history intact" $ do
      let result = runTestFS $ do
            _        <- createFile "scene.md"
            Ops.deleteFile "scene.md"
            present  <- Ops.exists "scene.md"
            ticks    <- Tick.fileTicksOf "scene.md"
            return (present, ticks)
      case result of
        Left err               -> expectationFailure err
        Right (present, ticks) -> do
          present `shouldBe` False
          map Tick.ftKind ticks `shouldBe` ["atom", "atom"]
          lookup "removed" (Tick.ftFields (last ticks)) `shouldBe` Just "true"

    -- The whole point of a forward-event delete: rebasing at a tick from
    -- before the deletion must still see the file exactly as it was then
    -- -- a rebase-based delete would have erased that history outright.
    -- 'readAt' only moves head, not the ambient tree (that's what
    -- 'inWorktree' is for), so checking historical presence needs both:
    -- jump there, then reset the ambient tree to match before reading it.
    it "a rebase to a tick before the deletion still sees the file present" $ do
      let result = runTestFS $ do
            tid0 <- createFile "scene.md"
            _    <- Ops.append "scene.md" "hello\n"
            Ops.deleteFile "scene.md"
            Core.readAt tid0 (Core.inWorktree (Ops.exists "scene.md"))
      case result of
        Left err         -> expectationFailure err
        Right stillThere -> stillThere `shouldBe` True

    -- 'atomHistory' (the content-fold 'Storage.Ops.commitFile'/
    -- 'commitWorktree' reconcile against) must stop at the most recent
    -- deletion marker, not fold pre-deletion content back in -- otherwise
    -- a path recreated after being deleted would appear to already
    -- contain its old content. The truncated history still includes the
    -- deletion marker itself (as the oldest entry of this "life") and the
    -- fresh creation marker after it -- both empty, so the fold's total
    -- content is what actually matters here, not the entry count.
    it "a path recreated after deletion starts genuinely empty, not carrying old content forward" $ do
      let result = runTestFS $ do
            _ <- createFile "scene.md"
            _ <- Ops.append "scene.md" "old content\n"
            Ops.deleteFile "scene.md"
            _ <- createFile "scene.md"
            Ops.atomHistory "scene.md"
      case result of
        Left err      -> expectationFailure err
        Right history -> mconcat (map snd history) `shouldBe` ""

    -- A fun consequence of deletion being an ordinary tick rather than a
    -- bespoke mechanism: the *existing* single-tick rebase this codebase
    -- already offers for correcting any misplaced tick (see
    -- 'Server.Core.File.deleteFileAtom'/'Storage.Ops.deleteTick') works on
    -- a removal tick for free -- removing it from history restores the
    -- file, with no dedicated "undo delete" feature needed anywhere.
    it "removing the deletion tick itself (via the ordinary single-tick rebase) restores the file" $ do
      let result = runTestFS $ do
            _      <- createFile "scene.md"
            _      <- Ops.append "scene.md" "hello\n"
            delTid <- Ops.deleteFile "scene.md"
            Ops.deleteTick delTid
            -- 'deleteTick' only moves head -- the ambient tree (what
            -- 'exists'\/'readFile' actually check) needs an explicit
            -- 'reset' to catch up, same as any other chain rebase.
            Core.reset
            present <- Ops.exists "scene.md"
            content <- Core.readFile "scene.md"
            return (present, content)
      case result of
        Left err                 -> expectationFailure err
        Right (present, content) -> do
          present `shouldBe` True
          content `shouldBe` "hello\n"

isOpaque :: Core.Tick -> Bool
isOpaque Core.Opaque {} = True
isOpaque _              = False
