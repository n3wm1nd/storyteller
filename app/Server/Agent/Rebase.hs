{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Agent.Rebase
  ( RebaseResp(..)
  , AgentRebaseReq(..)
  , handleBranchRebase
  , handleAgentRebase
  ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant (Handler)

import Server.Env (ServerEnv(..))
import Server.Run (runRequest)
import Server.Util (withBranch)
import Storyteller.Agent.SplitDiffMerge (splitDiffMerge)
import Storyteller.Git (BranchTag(..))

data Main

data RebaseResp = RebaseResp
  { rebaseRespRemapped :: Int
  } deriving (Show, Generic)

instance ToJSON RebaseResp
instance FromJSON RebaseResp

data AgentRebaseReq = AgentRebaseReq
  { agentRebaseBranch :: T.Text
  } deriving (Show, Generic)

instance FromJSON AgentRebaseReq
instance ToJSON AgentRebaseReq

handleBranchRebase :: ServerEnv -> T.Text -> Handler RebaseResp
handleBranchRebase env branch =
  handleAgentRebase env AgentRebaseReq { agentRebaseBranch = branch }

handleAgentRebase :: ServerEnv -> AgentRebaseReq -> Handler RebaseResp
handleAgentRebase env req = runRequest env $
  withBranch @Main env (agentRebaseBranch req) $ do
    mapping <- splitDiffMerge @(BranchTag Main) @Main
    return RebaseResp { rebaseRespRemapped = length mapping }
