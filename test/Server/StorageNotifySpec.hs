{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | 'Server.Writer.Run.storageNotify' is the one thing that decides whether
--   a rebase's tick-id remapping ever reaches a client as 'tick.remap'. An
--   in-place 'at' command's own tail replay renames every tail tick (new
--   parent, new hash), and a client tracking one of those ids (a rebase
--   marker, a context selection) needs to learn where it went.
--
--   That used to fall through a real gap: every command handler (see
--   'Server.Writer.File.Connection') runs its command under a fresh,
--   per-command 'Storyteller.Core.Git.withStorage', whose 'UpdateReferences'
--   case used to cascade entirely inside its own local buffer and never
--   re-emit anything outward for 'storageNotify' to see — only the
--   collapsed 'setRef' escaped, which 'storageNotify' doesn't react to at
--   all. 'Storyteller.AtGenericSpec' already proved 'withStorage' collapses
--   a rebase to one real ref write without 'storageNotify' wired in at all,
--   and 'Server.NotificationSpec' proved 'watchBranch' dispatches a
--   'TicksRemapped' that's already sitting in the channel — neither checked
--   whether one is actually posted there for a real command. The fix
--   ('cascadeReplace' returning what it discovers instead of discarding it,
--   threaded through 'withStorage' and announced via the dedicated
--   'AnnounceRemap' effect — see its own doc for why that's a separate
--   effect from 'UpdateReferences', not a reuse of it) is exercised here.
module Server.StorageNotifySpec (spec) where

import Prelude hiding (readFile, writeFile)

import Control.Concurrent.STM (TChan, atomically, newTChanIO, tryReadTChan)
import Data.Text (Text)
import Test.Hspec

import Polysemy
import Polysemy.Fail (Fail, runFail)
import Polysemy.State (evalState)

import Git.Mock (emptyGitState, runGitMock)
import Runix.Git (Git)

import Storyteller.Core.Branch (BranchOp, runStorage)
import Storyteller.Core.Git
  ( atGeneric, runBranchOpGit, runStoryStorageGit, withStorage )
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch, updateReferences)
import Storyteller.Core.Types (BranchName(..), TickId(..), branchHead, unTickId)
import Server.Writer.Notification (BranchNotification(..))
import Server.Writer.Run (storageNotify)
import qualified Storage.Core as Core

-- | Local phantom branch tag, same reasoning as 'Storyteller.AtGenericSpec':
--   keeps this test independent of 'Storyteller.Core.Runtime'.
data Main

mainBranch :: BranchName
mainBranch = BranchName "main"

-- | root <- a1 <- a2 <- a3 on 'mainBranch'. Returns (a1 = rebase target,
--   a2, a3 = the tail that gets popped and replayed back on top).
buildChain :: Members '[StoryStorage, Git, Fail] r => Sem r (Core.ObjectHash, Core.ObjectHash, Core.ObjectHash)
buildChain = do
  _ <- createBranch mainBranch
  (a1, _) <- runBranchOpGit @Main mainBranch (runStorage @Main (Core.store (Core.Atom [] "a.md" [] "A\n")))
  (a2, _) <- runBranchOpGit @Main mainBranch (runStorage @Main (Core.store (Core.Atom [] "a.md" [] "B\n")))
  (a3, _) <- runBranchOpGit @Main mainBranch (runStorage @Main (Core.store (Core.Atom [] "a.md" [] "C\n")))
  return (a1, a2, a3)

-- | Changes the working tree at the rebase target, so the replayed a2/a3
--   differ from the originals (new hashes, not an identity remap) — same
--   reasoning as 'Storyteller.AtGenericSpec.markerInner': without this the
--   replay reproduces identical content-addressed commits, and "was there a
--   real remap to report" wouldn't be a meaningful question.
markerInner :: forall r. Members '[BranchOp Main, Git, Fail] r => Sem r ()
markerInner = () <$ runStorage @Main (Core.store (Core.Atom [] "m.md" [] "M\n"))

drainChan :: TChan a -> IO [a]
drainChan chan = go []
  where
    go acc = atomically (tryReadTChan chan) >>= \case
      Just x  -> go (x : acc)
      Nothing -> return (reverse acc)

isTicksRemapped :: BranchNotification -> Bool
isTicksRemapped (TicksRemapped _) = True
isTicksRemapped _                 = False

-- | Every 'TicksRemapped' mapping entry, old id first, across all events —
--   in practice there's at most one such event per command (see
--   'Storyteller.AtGenericSpec's "cross-branch cascade fires exactly once"),
--   but this doesn't assume that; it just flattens whatever arrived.
remapPairs :: [BranchNotification] -> [(Text, Text)]
remapPairs events = [ pair | TicksRemapped pairs <- events, pair <- pairs ]

spec :: Spec
spec = describe "storageNotify" $ do

  it "posts a TicksRemapped covering the whole tail for an ordinary in-place 'at' command" $ do
    chan <- newTChanIO
    result <- runM . runFail . evalState emptyGitState . runGitMock . runStoryStorageGit . storageNotify chan $ do
      (a1, a2, a3) <- buildChain
      _ <- runFail $ withStorage $ runBranchOpGit @Main mainBranch $
        atGeneric @Main (TickId (Core.unObjectHash a1)) markerInner
      finalHead <- getBranch mainBranch
      return (a2, a3, finalHead)
    events <- drainChan chan
    case result of
      Left err -> expectationFailure err
      Right (a2, a3, mBranch) -> do
        let pairs = remapPairs events
        -- Both tail ticks were popped and replayed with new parents, so
        -- both must appear, each mapping to a real, resolvable id — not
        -- just "some remap event fired, with whatever content".
        lookup (Core.unObjectHash a2) pairs `shouldSatisfy` (/= Nothing)
        lookup (Core.unObjectHash a3) pairs `shouldSatisfy` (/= Nothing)
        -- a3 (the deepest tail tick, i.e. the original head) must resolve
        -- to the actual final head reported by StoryStorage afterwards —
        -- not to some intermediate id from partway through the replay.
        case mBranch of
          Just b  -> lookup (Core.unObjectHash a3) pairs `shouldBe` Just (unTickId (branchHead b))
          Nothing -> expectationFailure "branch vanished"

  it "does post TicksRemapped for an UpdateReferences call that reaches the real StoryStorage directly" $ do
    chan <- newTChanIO
    _ <- runM . runFail . evalState emptyGitState . runGitMock . runStoryStorageGit . storageNotify chan $ do
      _ <- createBranch mainBranch
      updateReferences [(TickId "old", TickId "new")]
    events <- drainChan chan
    filter isTicksRemapped events `shouldBe` [TicksRemapped [("old", "new")]]

  it "posts nothing for a no-op UpdateReferences (empty mapping)" $ do
    chan <- newTChanIO
    _ <- runM . runFail . evalState emptyGitState . runGitMock . runStoryStorageGit . storageNotify chan $ do
      _ <- createBranch mainBranch
      updateReferences []
    events <- drainChan chan
    filter isTicksRemapped events `shouldBe` []
