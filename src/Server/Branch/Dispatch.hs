{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Dispatch for /branch/{name} connections.
--
-- Routing only: decode BranchCommand → call Server.Branch → emit events.
-- No business logic lives here.
module Server.Branch.Dispatch
  ( dispatch
  , connectSnapshot
  ) where

import qualified Data.Text as T
import Data.Aeson (encode)
import qualified Network.WebSockets as WS
import Polysemy (runM)

import Server.Branch (branchState, addNote, moveTickInBranch, deleteTickFromBranch,
                      trackFiles, charGen, chatPrompt)
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

dispatch :: ServerEnv -> T.Text -> WS.Connection -> BranchCommand -> IO ()
dispatch env branch conn cmd = do
  let emit   = WS.sendTextData conn . encode
      name   = BranchName branch
      update = do
        mState <- branchState name
        return $ maybe (BranchError "branch not found") (BranchUpdate . snd) mState

  r <- runM $ actionStack env $ loggingWS conn $ case cmd of

    Track mid source files -> do
      paths <- trackFiles name (BranchName source) (map toPair files)
      return (map (FileAdded mid) paths, Nothing)

    CharGen mid path scenario seed -> do
      charGen name path scenario seed
      upd <- update
      return ([FileAdded mid path], Just upd)

    AddNote _mid refTickId text -> do
      addNote name (TickId refTickId) text
      upd <- update
      return ([], Just upd)

    MoveTick _mid tid mAfter -> do
      moveTickInBranch name (TickId tid) (TickId <$> mAfter)
      upd <- update
      return ([], Just upd)

    DeleteTick _mid tid -> do
      deleteTickFromBranch name (TickId tid)
      upd <- update
      return ([], Just upd)

    ChatPrompt _mid path prompt -> do
      chatPrompt name path prompt
      upd <- update
      return ([], Just upd)

  case r of
    Left err            -> emit (BranchError (T.pack err))
    Right (evts, mUpd) -> do
      mapM_ emit evts
      maybe (return ()) emit mUpd

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

toPair :: TrackFile -> (FilePath, FilePath)
toPair tf = (trackFrom tf, trackTo tf)
