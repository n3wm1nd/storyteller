{-# LANGUAGE FlexibleContexts #-}
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
  , watchBranch
  ) where

import qualified Data.Text as T
import Control.Concurrent.STM (TChan, atomically, readTChan)
import Polysemy (Embed, Member, Sem, embed)

-- | A story branch's ref moved; its tick chain may have changed.
newtype BranchNotification = BranchNotification
  { bnBranch :: T.Text
  } deriving (Show, Eq)

-- | Block on ref-move notifications for the connection's lifetime, running
--   'onMatch' whenever one names this connection's branch. Threads an
--   accumulator ('s') through — e.g. the last HEAD pushed — so a caller
--   diffing against "since last push" doesn't need to touch the channel
--   itself. Notifications for other branches are silently skipped.
watchBranch
  :: Member (Embed IO) r
  => TChan BranchNotification
  -> T.Text
  -> s
  -> (s -> Sem r s)
  -> Sem r s
watchBranch chan branch = loop
  where
    loop state onMatch = do
      note <- embed $ atomically (readTChan chan)
      state' <- if bnBranch note == branch then onMatch state else return state
      loop state' onMatch
