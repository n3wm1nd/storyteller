{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Dispatch for /session connections.
--
-- Routing only: decode SessionCommand → call Storyteller.Storage → push the
-- event. Runs against the ambient, already-open session scope — see
-- 'Server.Session.Connection' for where that scope is entered. Throws
-- (Error String) on failure — the caller catches it and turns it into a
-- SessionError push rather than ending the connection.
module Server.Session.Dispatch
  ( runCommand
  ) where

import Data.Aeson (encode)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Polysemy (Embed, Member, Sem, embed)
import Polysemy.Error (throw)

import Server.Run (SessionEffects)
import Server.Session.Protocol
import Storyteller.Storage (listBranches, createBranch, getBranch, deleteBranch)
import Storyteller.Types (BranchName(..), Branch(..))

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

  where
    push = embed . WS.sendTextData conn . encode
