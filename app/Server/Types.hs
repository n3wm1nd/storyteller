{-# LANGUAGE DeriveGeneric #-}

-- | Shared JSON types for branch/tick management and file access.
-- Agent-specific request/response types live in their own Server.Agent.* modules.
module Server.Types
  ( BranchParam
  , TickParam
  , BranchInfo(..)
  , CreateBranchReq(..)
  , TickInfo(..)
  , FileResp(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

type BranchParam = Text
type TickParam   = Text

data BranchInfo = BranchInfo
  { branchInfoName :: Text
  , branchInfoHead :: Text
  } deriving (Show, Generic)

instance ToJSON BranchInfo
instance FromJSON BranchInfo

data CreateBranchReq = CreateBranchReq
  { createBranchName :: Text
  } deriving (Show, Generic)

instance FromJSON CreateBranchReq
instance ToJSON CreateBranchReq

data TickInfo = TickInfo
  { tickInfoId          :: Text
  , tickInfoParent      :: Maybe Text
  , tickInfoLinkedTicks :: [Text]
  , tickInfoMessage     :: Text
  } deriving (Show, Generic)

instance ToJSON TickInfo
instance FromJSON TickInfo

data FileResp = FileResp
  { fileRespPath    :: Text
  , fileRespContent :: Text
  } deriving (Show, Generic)

instance ToJSON FileResp
instance FromJSON FileResp
