{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Dispatch for /session connections.
--
-- Handlers operate at storage level — no StoryBranch in scope.
module Server.Session.Dispatch
  ( dispatch
  ) where

import Data.Aeson (encode)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Server.Env (ServerEnv)
import Server.Run (runAction, SessionEffects)
import Server.Session.Protocol

import Polysemy (Sem)
import Storyteller.Storage (listBranches, createBranch, getBranch, deleteBranch)
import Storyteller.Types (BranchName(..), Branch(..))
import Polysemy.Error (throw)

dispatch :: ServerEnv -> WS.Connection -> SessionCommand -> IO ()
dispatch env conn cmd = do
  let emit = WS.sendTextData conn . encode
      orErr (Left err) _ = emit (SessionError (T.pack err))
      orErr (Right v)  f = emit (f v)

  case cmd of
    ListBranches mid -> do
      r <- runAction env handleListBranches
      orErr r (BranchList mid)

    CreateBranch mid branch -> do
      r <- runAction env (handleCreateBranch branch)
      orErr r (BranchCreated mid)

    DeleteBranch mid branch -> do
      r <- runAction env (handleDeleteBranch branch)
      orErr r (BranchDeleted mid)

-- ---------------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------------

handleListBranches :: SessionEffects r => Sem r [T.Text]
handleListBranches = map (unBranchName . branchName) <$> listBranches

handleCreateBranch :: SessionEffects r => T.Text -> Sem r T.Text
handleCreateBranch branch = do
  let name = BranchName branch
  getBranch name >>= \case
    Just _  -> throw ("branch already exists: " <> T.unpack branch)
    Nothing -> unBranchName . branchName <$> createBranch name

handleDeleteBranch :: SessionEffects r => T.Text -> Sem r T.Text
handleDeleteBranch branch = do
  let name = BranchName branch
  getBranch name >>= \case
    Nothing -> throw ("branch not found: " <> T.unpack branch)
    Just _  -> deleteBranch name >> return branch
