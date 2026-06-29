{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Top-level server wiring.
--
-- Two responsibilities:
--   1. Branch & tick management handlers (no dedicated agent module needed).
--   2. Wire all agent handlers into the Servant server.
--
-- Each agent group lives in its own Server.Agent.* module. To add a new
-- agent: implement it there, import the handler here, add it to the API type
-- in Server.API, and wire it into 'agentsServer' or 'branchesServer'.
module Server.Handlers
  ( server
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.List (intercalate)
import Polysemy
import Polysemy.Error (throw)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, fileExists, readFile)
import Servant

import Server.Agent.Append  (handleBranchAppend, handleAgentAppend)
import Server.Agent.CharGen (handleCharGen)
import Server.Agent.Rebase  (handleBranchRebase, handleAgentRebase)
import Server.Agent.Track   (handleBranchTrack, handleAgentTrack)
import Server.Agent.Write   (handleBranchWrite, handleAgentWrite)
import Server.API (API, BranchesAPI, AgentsAPI)
import Server.Env (ServerEnv(..))
import Server.Run (runRequest)
import Server.Types
import Server.Util (withBranch)

import Storyteller.Git (BranchTag(..))
import Storyteller.Storage ( StoryBranch, StoryStorage
                           , createBranch, getBranch, deleteBranch, listBranches, follow )
import Storyteller.Types ( BranchName(..), Branch(..), Tick(..), TickId(..) )

import Prelude hiding (readFile)

data Main

-- ---------------------------------------------------------------------------
-- Server
-- ---------------------------------------------------------------------------

server :: ServerEnv -> Server API
server env = branchesServer env :<|> agentsServer env

branchesServer :: ServerEnv -> Server BranchesAPI
branchesServer env
  =    handleListBranches   env
  :<|> handleCreateBranch   env
  :<|> handleDeleteBranch   env
  :<|> handleListTicks      env
  :<|> handleGetTick        env
  :<|> handleBranchAppend   env
  :<|> handleBranchReadFile env
  :<|> handleBranchWrite    env
  :<|> handleBranchTrack    env
  :<|> handleBranchRebase   env

agentsServer :: ServerEnv -> Server AgentsAPI
agentsServer env
  =    handleAgentAppend env
  :<|> handleAgentWrite  env
  :<|> handleAgentTrack  env
  :<|> handleAgentRebase env
  :<|> handleCharGen     env

-- ---------------------------------------------------------------------------
-- Branch & tick management
-- ---------------------------------------------------------------------------

handleListBranches :: ServerEnv -> Handler [BranchInfo]
handleListBranches env = runRequest env $
  map toBranchInfo <$> listBranches

handleCreateBranch :: ServerEnv -> CreateBranchReq -> Handler BranchInfo
handleCreateBranch env req = runRequest env $ do
  let name = BranchName (createBranchName req)
  getBranch name >>= \case
    Just _  -> throw "branch already exists"
    Nothing -> toBranchInfo <$> createBranch name

handleDeleteBranch :: ServerEnv -> BranchParam -> Handler NoContent
handleDeleteBranch env b = runRequest env $ do
  let name = BranchName b
  getBranch name >>= \case
    Nothing -> throw ("branch not found: " <> T.unpack b)
    Just _  -> deleteBranch name >> return NoContent

handleListTicks :: ServerEnv -> BranchParam -> Handler [TickInfo]
handleListTicks env b = runRequest env $
  withBranch @Main env b $
    fmap reverse $ follow @Main [] $ \acc tick ->
      (toTickInfo tick : acc, tickParent tick)

handleGetTick :: ServerEnv -> BranchParam -> TickParam -> Handler TickInfo
handleGetTick env b t = runRequest env $
  withBranch @Main env b $ do
    ticks <- fmap reverse $ follow @Main [] $ \acc tick ->
      (tick : acc, tickParent tick)
    case filter (\tk -> unTickId (tickId tk) == t) ticks of
      (tk:_) -> return (toTickInfo tk)
      []     -> throw ("tick not found: " <> T.unpack t)

-- ---------------------------------------------------------------------------
-- File read (no agent — read-only)
-- ---------------------------------------------------------------------------

handleBranchReadFile :: ServerEnv -> BranchParam -> [String] -> Handler FileResp
handleBranchReadFile env b pathParts = runRequest env $
  withBranch @Main env b $ do
    let path = intercalate "/" pathParts
    fileExists @(BranchTag Main) path >>= \case
      False -> throw ("file not found: " <> path)
      True  -> do
        content <- readFile @(BranchTag Main) path
        return FileResp
          { fileRespPath    = T.pack path
          , fileRespContent = TE.decodeUtf8 content
          }

-- ---------------------------------------------------------------------------
-- Conversion helpers
-- ---------------------------------------------------------------------------

toBranchInfo :: Branch -> BranchInfo
toBranchInfo br = BranchInfo
  { branchInfoName = unBranchName (branchName br)
  , branchInfoHead = unTickId (branchHead br)
  }

toTickInfo :: Tick -> TickInfo
toTickInfo tk = TickInfo
  { tickInfoId      = unTickId (tickId tk)
  , tickInfoParent  = unTickId <$> tickParent tk
  , tickInfoRefs    = map unTickId (tickRefs tk)
  , tickInfoMessage = tickMessage tk
  }
