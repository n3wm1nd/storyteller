{-# LANGUAGE OverloadedStrings #-}

-- | Pub/sub notification types broadcast to connected clients.
--
-- Any write operation that mutates branch state posts a 'BranchNotification'
-- to 'ServerEnv.envNotifyChan'. Open branch connections subscribe via
-- 'dupTChan' and forward notifications to their WebSocket clients.
module Server.Notification
  ( BranchNotification(..)
  , IdMapping
  ) where

import Data.Aeson
import qualified Data.Text as T

-- | An old→new tick id rename.
type IdMapping = (T.Text, T.Text)

-- | A branch's tick chain was mutated. 'bnMapping' lists every tick id that
--   changed as a result (old→new pairs). Clients may use this for targeted
--   cache updates or simply invalidate and refetch.
data BranchNotification = BranchNotification
  { bnBranch  :: T.Text
  , bnMapping :: [IdMapping]
  } deriving (Show)

instance ToJSON BranchNotification where
  toJSON bn = object
    [ "type"    .= ("branch.invalidated" :: T.Text)
    , "branch"  .= bnBranch bn
    , "mapping" .= map (\(a,b) -> object ["old" .= a, "new" .= b]) (bnMapping bn)
    ]
