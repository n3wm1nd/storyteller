{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Agent.Track
  ( TrackReq(..)
  , TrackResp(..)
  , TrackFile(..)
  , handleTrack
  ) where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant (Handler)

import Server.Env (ServerEnv(..))
import Server.Run (runRequest)
import Storyteller.Agent.Tracker (trackBranch)
import Storyteller.Git (BranchTag(..), runBranchAndFS)
import Storyteller.Storage (createBranch, getBranch)
import Storyteller.Types (BranchName(..), TickId(..))

data Source
data Tracker

data TrackFile = TrackFile
  { trackFrom :: FilePath
  , trackTo   :: FilePath
  } deriving (Show, Generic)

instance FromJSON TrackFile
instance ToJSON TrackFile

data TrackReq = TrackReq
  { trackSource :: T.Text
  , trackFiles  :: [TrackFile]
  } deriving (Show, Generic)

instance FromJSON TrackReq
instance ToJSON TrackReq

data TrackResp = TrackResp
  { trackRespTicks :: [T.Text]
  } deriving (Show, Generic)

instance ToJSON TrackResp
instance FromJSON TrackResp

handleTrack :: ServerEnv -> T.Text -> Bool -> TrackReq -> Handler TrackResp
handleTrack env branch _flag req = runRequest env $ do
  let target = BranchName branch
      source = BranchName (trackSource req)
      files  = map (\f -> (trackFrom f, trackTo f)) (trackFiles req)
  getBranch target >>= \case
    Nothing -> void $ createBranch target
    Just _  -> return ()
  tids <- runBranchAndFS @Source source
        $ runBranchAndFS @Tracker target
        $ trackBranch @Source @Tracker @(BranchTag Tracker) files
  return TrackResp { trackRespTicks = map unTickId tids }
