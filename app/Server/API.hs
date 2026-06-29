{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API type definition.
--
-- Three surfaces:
--   /branches                     branch & tick management + branch-scoped agents
--   /agents                       globally-scoped agents (branch is a parameter)
--
-- File writes always go through an agent — no raw write endpoint exists.
-- Branch-scoped routes are aliases: same handler, branch pre-applied from URL.
module Server.API
  ( API
  , BranchesAPI
  , AgentsAPI
  , api
  ) where

import Servant

import Server.Types
import Server.Agent.Append  (AppendReq, AppendResp, AgentAppendReq)
import Server.Agent.Write   (WriteReq, WriteResp, AgentWriteReq)
import Server.Agent.Track   (TrackReq, TrackResp, AgentTrackReq)
import Server.Agent.CharGen (CharGenReq, CharGenResp)

type API = BranchesAPI :<|> AgentsAPI

-- ---------------------------------------------------------------------------
-- /branches
-- ---------------------------------------------------------------------------

type BranchesAPI
  =    "branches" :> Get '[JSON] [BranchInfo]
  :<|> "branches" :> ReqBody '[JSON] CreateBranchReq :> Post '[JSON] BranchInfo
  :<|> "branches" :> Capture "branch" BranchParam :> Delete '[JSON] NoContent
  :<|> "branches" :> Capture "branch" BranchParam :> "ticks" :> Get '[JSON] [TickInfo]
  :<|> "branches" :> Capture "branch" BranchParam :> "ticks" :> Capture "tick" TickParam
       :> Get '[JSON] TickInfo
  :<|> "branches" :> Capture "branch" BranchParam
       :> "fs" :> CaptureAll "path" String
       :> "append" :> ReqBody '[JSON] AppendReq :> Post '[JSON] AppendResp
  :<|> "branches" :> Capture "branch" BranchParam
       :> "fs" :> CaptureAll "path" String
       :> Get '[JSON] FileResp
  :<|> "branches" :> Capture "branch" BranchParam
       :> "write" :> ReqBody '[JSON] WriteReq :> Post '[JSON] WriteResp
  :<|> "branches" :> Capture "branch" BranchParam
       :> "track" :> ReqBody '[JSON] TrackReq :> Post '[JSON] TrackResp

-- ---------------------------------------------------------------------------
-- /agents
-- ---------------------------------------------------------------------------

type AgentsAPI
  =    "agents" :> "append"  :> ReqBody '[JSON] AgentAppendReq  :> Post '[JSON] AppendResp
  :<|> "agents" :> "write"   :> ReqBody '[JSON] AgentWriteReq   :> Post '[JSON] WriteResp
  :<|> "agents" :> "track"   :> ReqBody '[JSON] AgentTrackReq   :> Post '[JSON] TrackResp
  :<|> "agents" :> "chargen" :> ReqBody '[JSON] CharGenReq      :> Post '[JSON] CharGenResp

api :: Proxy API
api = Proxy

