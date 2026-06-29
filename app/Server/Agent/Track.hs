{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Agent.Track
  ( TrackReq(..)
  , TrackResp(..)
  , AgentTrackReq(..)
  , handleBranchTrack
  , handleAgentTrack
  ) where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Polysemy
import Servant (Handler)

import Server.Env (ServerEnv(..))
import Server.Run (runRequest)
import Storyteller.Agent.Tracker (trackBranch)
import Storyteller.Git (BranchTag(..), runBranchAndFS)
import Storyteller.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Types (BranchName(..), TickId(..))

data Source
data Tracker

data TrackReq = TrackReq
  { trackSource :: T.Text
  , trackFiles  :: [FilePath]
  } deriving (Show, Generic)

instance FromJSON TrackReq
instance ToJSON TrackReq

data TrackResp = TrackResp
  { trackRespTicks :: [T.Text]
  } deriving (Show, Generic)

instance ToJSON TrackResp
instance FromJSON TrackResp

data AgentTrackReq = AgentTrackReq
  { agentTrackTarget :: T.Text
  , agentTrackSource :: T.Text
  , agentTrackFiles  :: [FilePath]
  } deriving (Show, Generic)

instance FromJSON AgentTrackReq
instance ToJSON AgentTrackReq

handleBranchTrack :: ServerEnv -> T.Text -> TrackReq -> Handler TrackResp
handleBranchTrack env branch req =
  handleAgentTrack env AgentTrackReq
    { agentTrackTarget = branch
    , agentTrackSource = trackSource req
    , agentTrackFiles  = trackFiles req
    }

handleAgentTrack :: ServerEnv -> AgentTrackReq -> Handler TrackResp
handleAgentTrack env req = runRequest env $ do
  let target = BranchName (agentTrackTarget req)
      source = BranchName (agentTrackSource req)
      files  = agentTrackFiles req
  getBranch target >>= \case
    Nothing -> void $ createBranch target
    Just _  -> return ()
  tids <- runBranchAndFS @Source source
        $ runBranchAndFS @Tracker target
        $ trackBranch @Source @(BranchTag Source) @Tracker @(BranchTag Tracker) files
  return TrackResp { trackRespTicks = map unTickId tids }
