{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Servant API type definition.
--
-- URL conventions:
--   /branches/{branch}/fs/{path}           file resource
--   /branches/{branch}/fs/{path}?{action}  action on file content
--   /branches/{branch}?{action}            action on branch
--
-- Common actions are flat query flags (?append, ?track).
-- Namespaced actions use dot-separation (?generate.random.chargen).
-- The query flag approach avoids path segment collisions with story filenames,
-- which may contain any characters.
module Server.API
  ( API
  , api
  ) where

import Servant

import Server.Types
import Server.Agent.Append  (AppendReq, AppendResp)
import Server.Agent.Track   (TrackReq, TrackResp)
import Server.Agent.CharGen (CharGenReq, CharGenResp)

type API
  -- Branch management
  =    "branches" :> Get '[JSON] [BranchInfo]
  :<|> "branches" :> ReqBody '[JSON] CreateBranchReq :> Post '[JSON] BranchInfo
  :<|> "branches" :> Capture "branch" BranchParam :> Delete '[JSON] NoContent

  -- Tick stream (raw storage view)
  :<|> "branches" :> Capture "branch" BranchParam
       :> "ticks" :> Get '[JSON] [TickInfo]
  :<|> "branches" :> Capture "branch" BranchParam
       :> "ticks" :> Capture "tick" TickParam :> Get '[JSON] TickInfo

  -- Branch-scoped actions
  :<|> "branches" :> Capture "branch" BranchParam
       :> QueryFlag "track" :> ReqBody '[JSON] TrackReq :> Post '[JSON] TrackResp

  -- File resource
  :<|> "branches" :> Capture "branch" BranchParam
       :> "fs" :> CaptureAll "path" String
       :> Get '[JSON] FileResp
  :<|> "branches" :> Capture "branch" BranchParam
       :> "fs" :> CaptureAll "path" String
       :> Delete '[JSON] NoContent

  -- File-scoped actions
  :<|> "branches" :> Capture "branch" BranchParam
       :> "fs" :> CaptureAll "path" String
       :> QueryFlag "append" :> ReqBody '[JSON] AppendReq :> Post '[JSON] AppendResp
  :<|> "branches" :> Capture "branch" BranchParam
       :> "fs" :> CaptureAll "path" String
       :> QueryFlag "generate.chargen" :> ReqBody '[JSON] CharGenReq :> Post '[JSON] CharGenResp

api :: Proxy API
api = Proxy
