{-# LANGUAGE OverloadedStrings #-}

-- | Pub/sub notification broadcast when a branch's ref moves.
--
-- 'gitNotify' posts a 'BranchNotification' to 'ServerEnv.envNotifyChan'
-- whenever a story branch ref is created or updated. Open branch and file
-- connections subscribe via 'dupTChan', check whether the notification
-- names their branch, and if so refetch and re-push their full state.
--
-- This is purely an internal signal — it never goes over the wire itself.
module Server.Notification
  ( BranchNotification(..)
  ) where

import qualified Data.Text as T

-- | A story branch's ref moved; its tick chain may have changed.
newtype BranchNotification = BranchNotification
  { bnBranch :: T.Text
  } deriving (Show, Eq)
