{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Agent.Write
  ( WriteReq(..)
  , WriteResp(..)
  , AgentWriteReq(..)
  , handleBranchWrite
  , handleAgentWrite
  ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant (Handler)

import Server.Env (ServerEnv(..))
import Server.Run (runRequest)
import Server.Util (withBranchLLM)
import Storyteller.Agent.Write (writeAgent)
import Storyteller.Git (BranchTag(..))
import Storyteller.Types (TickId(..))

data Main

data WriteReq = WriteReq
  { writeReqFile     :: FilePath
  , writeInstruction :: T.Text
  , writeActiveChars :: [T.Text]
  } deriving (Show, Generic)

instance FromJSON WriteReq
instance ToJSON WriteReq

data WriteResp = WriteResp
  { writeRespText  :: T.Text
  , writeRespTicks :: [T.Text]
  } deriving (Show, Generic)

instance ToJSON WriteResp
instance FromJSON WriteResp

data AgentWriteReq = AgentWriteReq
  { agentWriteBranch      :: T.Text
  , agentWriteFile        :: FilePath
  , agentWriteInstruction :: T.Text
  , agentWriteActiveChars :: [T.Text]
  } deriving (Show, Generic)

instance FromJSON AgentWriteReq
instance ToJSON AgentWriteReq

handleBranchWrite :: ServerEnv -> T.Text -> WriteReq -> Handler WriteResp
handleBranchWrite env branch req =
  handleAgentWrite env AgentWriteReq
    { agentWriteBranch      = branch
    , agentWriteFile        = writeReqFile req
    , agentWriteInstruction = writeInstruction req
    , agentWriteActiveChars = writeActiveChars req
    }

handleAgentWrite :: ServerEnv -> AgentWriteReq -> Handler WriteResp
handleAgentWrite env req = runRequest env $
  withBranchLLM @Main env (agentWriteBranch req) $ do
    (text, tids) <- writeAgent @(BranchTag Main) @Main
                      (agentWriteFile req) (agentWriteInstruction req)
                      (agentWriteActiveChars req)
    return WriteResp { writeRespText = text, writeRespTicks = map unTickId tids }
