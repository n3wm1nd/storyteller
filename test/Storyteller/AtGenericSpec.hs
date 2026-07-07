{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Tests for 'Storyteller.Core.Git.atGeneric' -- the generic, 'Sem'-level
--   rebase behind the rebase-marker feature, whose inner action runs
--   arbitrary effects (LLM calls, further dispatches) at an earlier tick's
--   position. 'atGeneric' descends one tick per 'runStorage' dispatch, runs
--   the inner action at the target, then replays the tail back on top -- so
--   internally it asks 'runBranchOpGit' to 'setRef' on every step.
--
--   What that amounts to /externally/ is entirely a property of the
--   'withStorage' transaction 'atGeneric' always runs under in production
--   (see the command handlers): every intermediate 'setRef' is buffered in
--   'withStorageWithCallback''s overlay, and only the final replayed head
--   is published -- one ref write reaching git, regardless of how many
--   descent\/replay steps ran. A failure mid-rebase publishes nothing at
--   all (the replay never runs). Both are checked here, plus an eager-mode
--   contrast proving the single write is the transaction collapsing real
--   intermediate writes, not 'atGeneric' happening to make only one.
module Storyteller.AtGenericSpec (spec) where

import Prelude hiding (readFile, writeFile)

import Data.Either (isLeft)
import Data.Text (Text)
import Test.Hspec

import Polysemy
import Polysemy.Fail (Fail(..), runFail)
import Polysemy.State (State, evalState, get, modify)

import Git.Mock (emptyGitState, runGitMock)
import Runix.Git

import Storyteller.Core.Types (BranchName(..), TickId(..), branchHead)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Branch (BranchOp, runStorage)
import Storyteller.Core.Git
  ( atGeneric, runBranchOpGit, runStoryStorageGit, storyRefPrefix, withStorage )
import qualified Storage.Core as Core

-- | Local phantom branch tag. 'atGeneric'/'runBranchOpGit'/'runStorage' are
--   polymorphic over it, so the test needn't import
--   "Storyteller.Core.Runtime"'s 'Main' (and the deps it drags in).
data Main

mainBranch :: BranchName
mainBranch = BranchName "main"

mainRef :: RefName
mainRef = RefName (storyRefPrefix <> "main")

-- | Every real (mock-)git ref mutation that reached the 'Git' effect, in
--   order. A forwarding interpreter: reads and object I\/O pass through
--   untouched, only ref mutations ('CreateRef'\/'UpdateRef'\/'DeleteRef')
--   are recorded -- so this captures what actually landed in git, never
--   the in-memory overlay 'withStorage' buffers writes in before its
--   end-of-transaction replay. Sits between 'runStoryStorageGit' and
--   'runGitMock' so it sees the 'UpdateRef' 'applyToGit' issues on replay.
type RefLog = [(RefName, Maybe ObjectHash)]

recordRefWrites :: Members '[Git, State RefLog] r => Sem (Git : r) a -> Sem r a
recordRefWrites = interpret $ \case
  CreateRef ref h      -> send (CreateRef ref h)  <* modify (++ [(ref, Just h)])
  UpdateRef ref h      -> send (UpdateRef ref h)  <* modify (++ [(ref, Just h)])
  DeleteRef ref        -> send (DeleteRef ref)    <* modify (++ [(ref, Nothing)])
  ResolveRef ref       -> send (ResolveRef ref)
  ListRefs p           -> send (ListRefs p)
  ReadCommit h         -> send (ReadCommit h)
  WriteCommit cd       -> send (WriteCommit cd)
  ReadObject h         -> send (ReadObject h)
  WriteObject obj      -> send (WriteObject obj)
  LookupPath t path    -> send (LookupPath t path)
  IsAncestorOfAny ts h -> send (IsAncestorOfAny ts h)

writesOn :: RefName -> RefLog -> [Maybe ObjectHash]
writesOn ref = fmap snd . filter ((== ref) . fst)

-- | Build root <- a1 <- a2 <- a3 on 'mainBranch' (head a3), eagerly: each
--   store publishes immediately, /before/ the ref-write recording window
--   opens. Returns (a1 = rebase target, a3 = original head).
buildChain
  :: Members '[StoryStorage, Git, Fail] r
  => Sem r (Core.ObjectHash, Core.ObjectHash)
buildChain = do
  _ <- createBranch mainBranch
  (a1, _) <- runBranchOpGit @Main mainBranch (runStorage @Main (Core.store (Core.Atom [] "a.md" "A\n")))
  _ <- runBranchOpGit @Main mainBranch (runStorage @Main (Core.store (Core.Atom [] "a.md" "B\n")))
  (a3, _) <- runBranchOpGit @Main mainBranch (runStorage @Main (Core.store (Core.Atom [] "a.md" "C\n")))
  return (a1, a3)

-- | Descend from head a3 to a1, run @inner@ there, replay a2\/a3 back --
--   under one 'withStorage'. Returns the original head, the ref writes
--   that reached git /during the transaction/ (setup writes excluded), the
--   transaction's own result ('Left' iff @inner@ failed -- an inner
--   'runFail' so a mid-rebase failure is observed as a value, not a
--   short-circuit that hides the ref log), and the branch head afterwards.
runUnderWithStorage
  :: (forall r. Members '[BranchOp Main, Git, Fail] r => Sem r ())
  -> Either String (Core.ObjectHash, [Maybe ObjectHash], Either String (), Maybe TickId)
runUnderWithStorage inner =
  run
  . runFail
  . evalState emptyGitState
  . evalState ([] :: RefLog)
  . runGitMock
  . recordRefWrites
  . runStoryStorageGit
  $ do
    (target, origHead) <- buildChain
    logBefore <- get @RefLog
    txRes  <- runFail $ withStorage $ runBranchOpGit @Main mainBranch $
               atGeneric @Main (TickId (Core.unObjectHash target)) inner
    logAfter  <- get @RefLog
    finalHead <- (branchHead <$>) <$> getBranch mainBranch
    return (origHead, writesOn mainRef (drop (length logBefore) logAfter), txRes, finalHead)

-- | Same rebase, but with NO 'withStorage' -- eager mode, where every
--   internal 'setRef' lands in git immediately. Returns the ref writes
--   made during the rebase itself (setup excluded).
runEager
  :: (forall r. Members '[BranchOp Main, Git, Fail] r => Sem r ())
  -> Either String [Maybe ObjectHash]
runEager inner =
  run
  . runFail
  . evalState emptyGitState
  . evalState ([] :: RefLog)
  . runGitMock
  . recordRefWrites
  . runStoryStorageGit
  $ do
    (target, _origHead) <- buildChain
    logBefore <- get @RefLog
    _ <- runBranchOpGit @Main mainBranch $
           atGeneric @Main (TickId (Core.unObjectHash target)) inner
    logAfter <- get @RefLog
    return (writesOn mainRef (drop (length logBefore) logAfter))

-- | 'Just' the hash text a single recorded write landed, for comparing a
--   ref value against the 'TickId' 'getBranch' reports.
singleWriteText :: [Maybe ObjectHash] -> Maybe Text
singleWriteText [Just h] = Just (unObjectHash h)
singleWriteText _        = Nothing

spec :: Spec
spec = describe "atGeneric" $ do

  it "under withStorage, publishes exactly one ref write (the final replayed head) despite one setRef per internal step" $
    case runUnderWithStorage (pure ()) of
      Left err -> expectationFailure err
      Right (_origHead, mainWrites, txRes, finalHead) -> do
        txRes `shouldBe` Right ()
        length mainWrites `shouldBe` 1
        singleWriteText mainWrites `shouldBe` fmap unTickId finalHead

  it "without withStorage (eager), the same rebase publishes a ref write at every internal step" $
    case runEager (pure ()) of
      Left err -> expectationFailure err
      Right mainWrites -> mainWrites `shouldSatisfy` ((> 1) . length)

  it "under withStorage, a failure mid-rebase publishes nothing and leaves the head untouched" $
    case runUnderWithStorage (send (Fail "boom")) of
      Left err -> expectationFailure err
      Right (origHead, mainWrites, txRes, finalHead) -> do
        txRes `shouldSatisfy` isLeft
        mainWrites `shouldBe` []
        fmap unTickId finalHead `shouldBe` Just (Core.unObjectHash origHead)
