{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Agent.Append
  ( AppendReq(..)
  , AppendResp(..)
  , handleAppend
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

handleAppend :: ServerEnv -> T.Text -> [String] -> Bool -> AppendReq -> Handler AppendResp
handleAppend env branch pathParts _flag req =
  runRequest env $
    withBranchSplitter @Main env branch $ do
      let path = intercalate "/" pathParts
      tids <- appendAgent @(BranchTag Main) @Main path (appendContent req)
      return AppendResp { appendTicks = map unTickId tids }
