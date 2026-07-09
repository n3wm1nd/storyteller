{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pub/sub notification broadcast when a branch's ref moves, when a
-- rebase/replace/move rewrites tick ids and updates cross-branch references
-- accordingly, or when the undo log gets a new entry.
--
-- 'RefMoved' is posted by 'Server.Writer.GitWorker' whenever a story
-- branch ref is created or updated; connections filter it to their own
-- branch, then refetch and re-push their state. 'TicksRemapped' is posted by
-- 'Server.Writer.Run.storageNotify' whenever 'Storyteller.Core.Storage.updateReferences'
-- runs with a non-empty mapping; it is delivered to every connection
-- regardless of branch, since applying a remap to ids you aren't tracking is
-- a no-op — the client checks whether it holds any of the affected ids
-- (rebase marker, context selection) and updates them in place. 'UndoMoved'
-- is posted by 'Server.Writer.GitWorker' whenever 'Storyteller.Core.Undo's
-- own log ref moves -- a real write's 'Snapshot' always follows the write
-- it's recording, so this fires strictly after (never racing) the 'RefMoved'
-- for whatever branch that write touched; only 'Server.Writer.Session.Connection'
-- has any use for it, everyone else just ignores it, same as 'TicksRemapped'.
--
-- This is purely an internal signal — none of these go over the wire as
-- such; 'TicksRemapped' is forwarded to clients as a 'tick.remap' event, and
-- 'RefMoved'/'UndoMoved' each prompt a connection to re-push its own
-- wire-shaped state instead of being relayed directly.
module Server.Writer.Notification
  ( BranchNotification(..)
  , watchBranch
  ) where

import qualified Data.Text as T
import Control.Concurrent.STM (TChan, atomically, readTChan)
import Polysemy (Embed, Member, Sem, embed)

data BranchNotification
  = RefMoved      T.Text
  | TicksRemapped [(T.Text, T.Text)]
  | UndoMoved
  deriving (Show, Eq)

-- | Block on notifications for the connection's lifetime, running 'onMatch'
--   on every 'TicksRemapped' and on every 'RefMoved' naming this connection's
--   branch. Threads an accumulator ('s') through — e.g. the last HEAD pushed
--   — so a caller diffing against "since last push" doesn't need to touch
--   the channel itself. 'RefMoved' for other branches is silently skipped.
watchBranch
  :: Member (Embed IO) r
  => TChan BranchNotification
  -> T.Text
  -> s
  -> (s -> BranchNotification -> Sem r s)
  -> Sem r s
watchBranch chan branch = loop
  where
    loop state onMatch = do
      note <- embed $ atomically (readTChan chan)
      state' <- case note of
        RefMoved b | b /= branch -> return state
        _                        -> onMatch state note
      loop state' onMatch
