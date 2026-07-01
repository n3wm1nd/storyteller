{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Dispatch for /branch/{name} connections.
--
-- Each command handler is a pure-ish function (effects only from the effect
-- stack, no IO/WS concerns) that returns a BranchEvent. Dispatch routes
-- commands to handlers and emits the result. Connection setup lives in
-- Server.Branch.Connection.
module Server.Branch.Dispatch
  ( dispatch
  , connectSnapshot
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (encode)
import qualified Data.Yaml as Yaml
import qualified Network.WebSockets as WS
import Polysemy (Members, Sem, runM)
import Polysemy.Error (throw)
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, listAllFiles)
import Runix.Logging (info)

import Server.Branch.Protocol
import Server.Env (ServerEnv(..))
import Server.Protocol (Update(..), tickToWireTick)
import Server.Run (runAction, actionStack, loggingWS, SessionEffects)
import Server.Util (withBranch, withBranchSplitter)

import Storyteller.Agent (Prompt(..), Instruction(..))
import Storyteller.Agent.CharGen (charGenCommit, ScenarioTemplate(..), RngSeed(..))
import Storyteller.Agent.Tracker (trackBranch)
import Storyteller.Agent.Write (writeAgent)
import Storyteller.Edit (deleteTick, moveTick)
import Storyteller.Git (BranchTag(..), runBranchAndFS)
import Storyteller.Storage (StoryBranch, createBranch, getBranch, follow, get, storeAs)
import Storyteller.Types (BranchName(..), TickId(..), Note(..), tickId, tickParent, unTickId)


data Main
data Source
data Tracker
data CharBranch

-- ---------------------------------------------------------------------------
-- Connect snapshot
-- ---------------------------------------------------------------------------

-- | Full state push on connect: file list and full tick chain as an Update.
--   Returns Left on hard error, Right (files, update) on success.
--   If the branch doesn't exist yet, returns empty lists.
connectSnapshot :: ServerEnv -> T.Text -> IO (Either String ([FilePath], Maybe Update))
connectSnapshot env branch = runAction env $ do
  getBranch (BranchName branch) >>= \case
    Nothing -> return ([], Nothing)
    Just _  -> withBranch @Main branch $ do
      files  <- branchFiles @Main
      update <- branchUpdate @Main
      return (files, Just update)

-- ---------------------------------------------------------------------------
-- Dispatch
-- ---------------------------------------------------------------------------

dispatch :: ServerEnv -> T.Text -> WS.Connection -> BranchCommand -> IO ()
dispatch env branch conn cmd = do
  let emit = WS.sendTextData conn . encode
  r <- runM $ actionStack env $ loggingWS conn $ case cmd of
    Track mid source files -> do
      paths <- handleTrack branch source files
      return (map (FileAdded mid) paths, Nothing)

    CharGen mid path scenario seed -> do
      handleCharGen branch path scenario seed
      upd <- runUpdate branch
      return ([FileAdded mid path], Just upd)

    AddNote _mid refTickId noteText_ -> do
      handleAddNote branch refTickId noteText_
      upd <- runUpdate branch
      return ([], Just upd)

    MoveTick _mid tickId_ mAfterTickId -> do
      handleMoveTick branch tickId_ mAfterTickId
      upd <- runUpdate branch
      return ([], Just upd)

    DeleteTick _mid tickId_ -> do
      handleDeleteTick branch tickId_
      upd <- runUpdate branch
      return ([], Just upd)

    ChatPrompt _mid path promptText_ -> do
      handleChatPrompt branch path promptText_
      upd <- runUpdate branch
      return ([], Just upd)

  case r of
    Left err             -> emit (BranchError (T.pack err))
    Right (evts, mUpd)  -> do
      mapM_ emit evts
      maybe (return ()) (emit . BranchUpdate) mUpd

-- ---------------------------------------------------------------------------
-- Handlers — pure-ish, no WS concerns
-- ---------------------------------------------------------------------------

handleTrack :: SessionEffects r => T.Text -> T.Text -> [TrackFile] -> Sem r [FilePath]
handleTrack branch source files = do
  let target     = BranchName branch
      sourceName = BranchName source
      filePairs  = map (\f -> (trackFrom f, trackTo f)) files
      destPaths  = map snd filePairs
  getBranch target >>= \case
    Nothing -> void $ createBranch target
    Just _  -> return ()
  runBranchAndFS @Source sourceName
    $ runBranchAndFS @Tracker target $ do
        mapM_ (trackBranch @Source @Tracker) filePairs
        return destPaths

handleCharGen :: SessionEffects r => T.Text -> FilePath -> T.Text -> Maybe Int -> Sem r ()
handleCharGen branch path scenario seed = do
  let name = BranchName branch
  template <- case Yaml.decodeEither' (TE.encodeUtf8 scenario) of
    Left  err -> throw (Yaml.prettyPrintParseException err)
    Right val -> return (ScenarioTemplate val)
  getBranch name >>= \case
    Nothing -> void $ createBranch name
    Just _  -> return ()
  runBranchAndFS @CharBranch name $
    void $ charGenCommit @CharBranch template (RngSeed <$> seed) path

handleAddNote :: SessionEffects r => T.Text -> T.Text -> T.Text -> Sem r ()
handleAddNote branch refTickIdTxt text =
  withBranch @Main branch $ do
    let refId = TickId refTickIdTxt
    ticks <- follow @Main [] $ \acc t -> (t : acc, tickParent t)
    case filter (\t -> tickId t == refId) ticks of
      [] -> fail $ "ref tick not found in branch: " <> T.unpack refTickIdTxt
      _  -> void $ storeAs @Main (Note refId text)

handleMoveTick :: SessionEffects r => T.Text -> T.Text -> Maybe T.Text -> Sem r ()
handleMoveTick branch tickIdTxt mAfterTickIdTxt =
  withBranch @Main branch $ do
    let tid    = TickId tickIdTxt
        mAfter = TickId <$> mAfterTickIdTxt
    void $ moveTick @Main tid mAfter

handleDeleteTick :: SessionEffects r => T.Text -> T.Text -> Sem r ()
handleDeleteTick branch tickIdTxt =
  withBranch @Main branch $
    void $ deleteTick @Main (TickId tickIdTxt)

handleChatPrompt :: SessionEffects r => T.Text -> FilePath -> T.Text -> Sem r ()
handleChatPrompt branch path promptText_ =
  withBranchSplitter @Main branch $ do
    _ <- storeAs @Main (Prompt path promptText_)
    info $ "writer agent starting: " <> T.pack path
    _ <- writeAgent @(BranchTag Main) @Main path (Instruction promptText_) []
    info $ "writer agent done: " <> T.pack path

-- ---------------------------------------------------------------------------
-- Update builders
-- ---------------------------------------------------------------------------

-- | Build a full branch Update after any mutation.
runUpdate :: SessionEffects r => T.Text -> Sem r Update
runUpdate branch =
  getBranch (BranchName branch) >>= \case
    Nothing -> return (Update [] "")
    Just _  -> withBranch @Main branch $ branchUpdate @Main

-- | Assemble an Update from the current branch state.
--   Tick chain is oldest-first; head is the most recent tick id.
branchUpdate
  :: forall branch r
  .  Members '[StoryBranch branch, Fail] r
  => Sem r Update
branchUpdate = do
  ticks  <- follow @branch [] $ \acc t -> (t : acc, tickParent t)
  headTick <- get @branch
  let headId = unTickId (tickId headTick)
  return (Update (map tickToWireTick ticks) headId)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

branchFiles
  :: forall branch r
  .  Members '[FileSystem (BranchTag branch), Fail] r
  => Sem r [FilePath]
branchFiles = listAllFiles @(BranchTag branch) "/"
