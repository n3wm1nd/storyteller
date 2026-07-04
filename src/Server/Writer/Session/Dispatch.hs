{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Dispatch for /session connections.
--
-- Routing only: decode SessionCommand → call Storyteller.Core.Storage → push the
-- event. Runs against the ambient, already-open session scope — see
-- 'Server.Writer.Session.Connection' for where that scope is entered. Throws
-- (Error String) on failure — the caller catches it and turns it into a
-- SessionError push rather than ending the connection.
module Server.Writer.Session.Dispatch
  ( runCommand
  , characterSummaries
  ) where

import Data.Aeson (encode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.WebSockets as WS
import Polysemy (Embed, Member, Sem, embed)
import Polysemy.Error (throw)
import Runix.FileSystem (fileExists, readFile)

import Server.Core.Run (SessionEffects)
import Server.Writer.Session.Protocol
import Storyteller.Core.Git (BranchTag, runBranchAndFS)
import Storyteller.Core.Storage (listBranches, createBranch, getBranch, deleteBranch)
import Storyteller.Core.Types (BranchName(..), Branch(..))

import Prelude hiding (readFile)

runCommand :: (SessionEffects r, Member (Embed IO) r) => WS.Connection -> SessionCommand -> Sem r ()
runCommand conn cmd = case cmd of

  ListBranches mid -> do
    names <- map (unBranchName . branchName) <$> listBranches
    push (BranchList mid names)

  CreateBranch mid branch -> do
    let name = BranchName branch
    getBranch name >>= \case
      Just _  -> throw @String ("branch already exists: " <> T.unpack branch)
      Nothing -> do
        b <- createBranch name
        push (BranchCreated mid (unBranchName (branchName b)))

  DeleteBranch mid branch -> do
    let name = BranchName branch
    getBranch name >>= \case
      Nothing -> throw @String ("branch not found: " <> T.unpack branch)
      Just _  -> deleteBranch name >> push (BranchDeleted mid branch)

  ListCharacters mid -> do
    summaries <- characterSummaries
    push (CharacterList mid summaries)

  where
    push = embed . WS.sendTextData conn . encode

data SummaryBranch

-- | Every 'character/*' branch, each with its raw @sheet.md@ content (if
--   any) — shared by the 'ListCharacters' command above and the
--   connection's notifier (see 'Server.Writer.Session.Connection'), which
--   re-pushes this same list whenever a matching branch ref moves. Opens
--   each branch's own transient FS scope to read its sheet, the same way
--   'Server.Writer.Branch.trackFiles' opens a scope for a branch other than
--   the one already ambiently open.
characterSummaries :: SessionEffects r => Sem r [CharacterSummary]
characterSummaries = do
  names <- filter ("character/" `T.isPrefixOf`) . map (unBranchName . branchName) <$> listBranches
  mapM readSummary names
  where
    readSummary branch = runBranchAndFS @SummaryBranch (BranchName branch) $ do
      sheet <- fileExists @(BranchTag SummaryBranch) "sheet.md" >>= \case
        False -> return Nothing
        True  -> Just . TE.decodeUtf8 <$> readFile @(BranchTag SummaryBranch) "sheet.md"
      return (CharacterSummary branch sheet)
