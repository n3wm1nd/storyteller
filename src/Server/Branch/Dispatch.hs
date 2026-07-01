{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Dispatch for /branch/{name} connections.
--
-- Routing only: decode BranchCommand → call Server.Branch → emit events.
-- No business logic lives here.
--
-- Dispatch does not push tick state after a mutation: every mutation that
-- succeeds moves the branch's git ref, which 'Server.Run.gitNotify' turns
-- into a broadcast that this connection's own notify listener picks up like
-- any other write. Dispatch only reports command-specific structural events
-- (FileAdded) and immediate failures — things a ref move can't tell you.
module Server.Branch.Dispatch
  ( dispatch
  , connectSnapshot
  , notifyUpdate
  ) where

import qualified Data.Text as T
import Data.Aeson (encode)
import qualified Network.WebSockets as WS
import Polysemy (runM)

import Server.Branch (branchState, branchStateSince, addNote, moveTickInBranch,
                      deleteTickFromBranch, trackFiles, charGen, chatPrompt)
import Server.Branch.Protocol
import Server.Env (ServerEnv(..))
import Server.Protocol (Update)
import Server.Run (runAction, actionStack, loggingWS)

import Storyteller.Types (BranchName(..), TickId(..))

-- ---------------------------------------------------------------------------
-- Connect snapshot
-- ---------------------------------------------------------------------------

connectSnapshot :: ServerEnv -> T.Text -> IO (Either String ([FilePath], Maybe Update))
connectSnapshot env branch = runAction env $ do
  mState <- branchState (BranchName branch)
  return $ case mState of
    Nothing           -> ([], Nothing)
    Just (files, upd) -> (files, Just upd)

-- ---------------------------------------------------------------------------
-- Dispatch
-- ---------------------------------------------------------------------------

-- | Run a command and push only what a ref move can't convey: immediate
--   failures, and structural events like a newly tracked/generated file.
dispatch :: ServerEnv -> T.Text -> WS.Connection -> BranchCommand -> IO ()
dispatch env branch conn cmd = do
  let emit = WS.sendTextData conn . encode
      name = BranchName branch

  r <- runM $ actionStack env $ loggingWS conn $ case cmd of

    Track mid source files ->
      map (FileAdded mid) <$> trackFiles name (BranchName source) (map toPair files)

    CharGen mid path scenario seed -> do
      charGen name path scenario seed
      return [FileAdded mid path]

    AddNote _mid refTickId text -> do
      addNote name (TickId refTickId) text
      return []

    MoveTick _mid tid mAfter -> do
      moveTickInBranch name (TickId tid) (TickId <$> mAfter)
      return []

    DeleteTick _mid tid -> do
      deleteTickFromBranch name (TickId tid)
      return []

    ChatPrompt _mid path prompt -> do
      chatPrompt name path prompt
      return []

  case r of
    Left err   -> emit (BranchError (T.pack err))
    Right evts -> mapM_ emit evts

-- | Fetch an incremental branch update triggered by a ref-move notification
--   — the sole path by which tick state reaches a branch connection, whether
--   the write came from this connection, another one, or a background agent.
--
--   'Left' here is a genuine failure (storage/git error) and must reach the
--   client as a BranchError — folding it into "nothing changed" would hide
--   it. 'Right Nothing' means the branch itself doesn't exist (not an error
--   condition in this app), so there's nothing to push.
notifyUpdate :: ServerEnv -> T.Text -> Maybe TickId -> IO (Either String (Maybe Update))
notifyUpdate env branch since = do
  r <- runAction env (branchStateSince (BranchName branch) since)
  return $ fmap (fmap snd) r

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

toPair :: TrackFile -> (FilePath, FilePath)
toPair tf = (trackFrom tf, trackTo tf)
