{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Agent.Append
  ( AppendReq(..)
  , AppendResp(..)
  , AgentAppendReq(..)
  , handleBranchAppend
  , handleAgentAppend
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (intercalate)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant (Handler)

import Server.Env (ServerEnv(..))
import Server.Run (runRequest)
import Server.Util (withBranchSplitter)
import Storyteller.Agent.Append (appendAgent)
import Storyteller.Git (BranchTag(..))
import Storyteller.Types (TickId(..))

data Main

data AppendReq = AppendReq
  { appendContent :: T.Text
  } deriving (Show, Generic)

instance FromJSON AppendReq
instance ToJSON AppendReq

data AppendResp = AppendResp
  { appendTicks :: [T.Text]
  } deriving (Show, Generic)

instance ToJSON AppendResp
instance FromJSON AppendResp

data AgentAppendReq = AgentAppendReq
  { agentAppendBranch  :: T.Text
  , agentAppendFile    :: FilePath
  , agentAppendContent :: T.Text
  } deriving (Show, Generic)

instance FromJSON AgentAppendReq
instance ToJSON AgentAppendReq

handleBranchAppend :: ServerEnv -> T.Text -> [String] -> AppendReq -> Handler AppendResp
handleBranchAppend env branch pathParts req =
  handleAgentAppend env AgentAppendReq
    { agentAppendBranch  = branch
    , agentAppendFile    = intercalate "/" pathParts
    , agentAppendContent = appendContent req
    }

handleAgentAppend :: ServerEnv -> AgentAppendReq -> Handler AppendResp
handleAgentAppend env req = runRequest env $
  withBranchSplitter @Main env (agentAppendBranch req) $ do
    tids <- appendAgent @(BranchTag Main) @Main
              (agentAppendFile req) (agentAppendContent req)
    return AppendResp { appendTicks = map unTickId tids }
