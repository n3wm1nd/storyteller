{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Dispatch for /branch/{name}/{path} connections.
--
-- Routing only: decode FileCommand → call Server.File → emit events.
-- No business logic lives here.
--
-- Dispatch does not push tick state after a mutation: every mutation that
-- succeeds moves the branch's git ref, which 'Server.Run.gitNotify' turns
-- into a broadcast that this connection's own notify listener picks up like
-- any other write. Dispatch only reports immediate failures — the one thing
-- a ref move can't tell you.
module Server.File.Dispatch
  ( dispatch
  , connectSnapshot
  , notifyUpdate
  ) where

import qualified Data.Text as T
import Data.Aeson (encode)
import qualified Network.WebSockets as WS
import Polysemy (runM)
import Polysemy.Error (throw)

import Server.File (fileState, fileStateSince, appendToFile, editFileAtom, deleteFileAtom, moveFileAtom)
import Server.File.Protocol (FileCommand(..), FileEvent(..))
import Server.Env (ServerEnv)
import Server.Protocol (Update(..))
import Server.Run (runAction, actionStack, loggingWS)

import Storyteller.Types (BranchName(..), TickId(..))

-- ---------------------------------------------------------------------------
-- Connect snapshot
-- ---------------------------------------------------------------------------

connectSnapshot :: ServerEnv -> T.Text -> FilePath -> IO (Either String FileEvent, Maybe FileEvent)
connectSnapshot env branch path = do
  r <- runAction env (fileState (BranchName branch) path)
  return $ case r of
    Left err         -> (Left err, Nothing)
    Right Nothing    -> (Right (FileAbsent Nothing), Nothing)
    Right (Just upd) ->
      if null (updateTicks upd)
        then (Right (FileAbsent Nothing), Nothing)
        else (Right (FilePresent Nothing), Just (FileUpdate upd))

-- ---------------------------------------------------------------------------
-- Dispatch
-- ---------------------------------------------------------------------------

-- | Run a command and report only what a ref move can't convey: immediate
--   failures. Successful mutations reach the client via the ref-move
--   notification, same as anyone else's write. One interpreter stack launch
--   for the whole command, written on the happy path — "not yet implemented"
--   is just another 'throw' into the same 'Error String' the rest of the
--   stack already uses, not a separate short-circuit before the stack runs.
dispatch :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> FileCommand -> IO ()
dispatch env branch path conn cmd = do
  let emit = WS.sendTextData conn . encode
      name = BranchName branch

  r <- runM $ actionStack env $ loggingWS conn $ case cmd of

    Append _mid content ->
      appendToFile name path content

    Delete _mid ->
      throw @String "file delete not yet implemented"

    EditAtom _mid tid content ->
      editFileAtom name path (TickId tid) content

    DeleteAtom _mid tid ->
      deleteFileAtom name path (TickId tid)

    MoveAtom _mid tid mAfter ->
      moveFileAtom name path (TickId tid) (TickId <$> mAfter)

  case r of
    Left err -> emit (FileError (T.pack err))
    Right () -> return ()

-- | Fetch state for a ref-move notification — the sole path by which tick
--   state reaches a file connection, whether the write came from this
--   connection, another one, or a background agent.
--
--   'since = Nothing' means this connection is still in the absent state
--   from connect — mirror 'connectSnapshot' exactly, so it transitions to
--   present the moment the file gets its first tick.
--
--   'since = Just tid' means the connection already has a HEAD to diff
--   against — fetch only newer ticks, and produce no update at all if this
--   particular write didn't touch this file's chain.
--
--   'Left' here is a genuine failure and must reach the client as a
--   FileError, same as any command-triggered failure — folding it into "no
--   new ticks" would hide it. A missing branch is not an error condition in
--   this app (mirrors 'fileState': absent branch reads the same as an absent
--   file), so it resolves to no event/update rather than 'Left'.
notifyUpdate :: ServerEnv -> T.Text -> FilePath -> Maybe T.Text -> IO (Either String (Maybe FileEvent, Maybe Update))
notifyUpdate env branch path Nothing = do
  (evt, mFileUpd) <- connectSnapshot env branch path
  return $ case evt of
    Left err -> Left err
    Right e  -> Right (Just e, unwrap mFileUpd)
  where
    unwrap (Just (FileUpdate u)) = Just u
    unwrap _                     = Nothing
notifyUpdate env branch path since@(Just _) = do
  r <- runAction env (fileStateSince (BranchName branch) path since)
  return $ case r of
    Left err                                    -> Left err
    Right Nothing                                -> Right (Nothing, Nothing)
    Right (Just upd) | null (updateTicks upd)    -> Right (Nothing, Nothing)
                      | otherwise                -> Right (Nothing, Just upd)
