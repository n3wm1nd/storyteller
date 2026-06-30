{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Dispatch for /branch/{name} connections.
--
-- Handles branch-level operations: file tree snapshot on connect, track, chargen,
-- note ticks, and tick move/delete.
-- Per-file operations (append, read, delete) live in Server.File.Dispatch.
module Server.Branch.Dispatch
  ( dispatch
  , snapshot
  , tickSnapshot
  , notify
  ) where

import Control.Concurrent.STM (atomically, writeTChan)
import Control.Monad (void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (encode)
import qualified Data.Yaml as Yaml
import qualified Network.WebSockets as WS
import Polysemy (Members, Sem)
import Polysemy.Error (throw)
import Polysemy.Fail (Fail)

import Server.Branch.Protocol
import Server.Env (ServerEnv(..))
import Server.Notification (BranchNotification(..))
import Server.Run (runAction, SessionEffects)
import Server.Util (withBranch)

import Storyteller.Agent.CharGen (charGenCommit, ScenarioTemplate(..), RngSeed(..))
import Storyteller.Agent.Tracker (trackBranch)
import Storyteller.Edit (deleteTick, moveTick)
import Storyteller.Git (BranchTag(..), WorkingTree, FSNode(..), runBranchAndFS, loadWorkingTree)
import Storyteller.Storage (StoryBranch, StoryStorage, createBranch, getBranch, follow, storeAs)
import Storyteller.Types (BranchName(..), TickId(..), Tick(..), TickData(..), Note(..), TickType(..), tickId, tickParent)

import Runix.Git (Git, ObjectHash(..))


data Main
data Source
data Tracker
data CharBranch

-- ---------------------------------------------------------------------------
-- Dispatch
-- ---------------------------------------------------------------------------

dispatch :: ServerEnv -> T.Text -> WS.Connection -> BranchCommand -> IO ()
dispatch env branch conn cmd = do
  let emit = WS.sendTextData conn . encode

  case cmd of
    Track mid source files -> do
      r <- runAction env (handleTrack branch source files)
      case r of
        Left err -> emit (BranchError (T.pack err))
        Right ps -> mapM_ (\p -> emit (FileAdded mid p)) ps

    CharGen mid path scenario seed -> do
      r <- runAction env (handleCharGen branch path scenario seed)
      case r of
        Left err -> emit (BranchError (T.pack err))
        Right _  -> emit (FileAdded mid path)

    ReadTicks _mid -> do
      ticks <- tickSnapshot env branch
      case ticks of
        Left err -> emit (BranchError (T.pack err))
        Right ts -> emit (BranchTicks ts)

    AddNote mid refTickId noteText_ -> do
      r <- runAction env (handleAddNote branch refTickId noteText_)
      case r of
        Left err   -> emit (BranchError (T.pack err))
        Right tick -> do
          notify env branch []
          emit (BranchTicks [tick])

    MoveTick mid tickId_ mAfterTickId -> do
      r <- runAction env (handleMoveTick branch tickId_ mAfterTickId)
      case r of
        Left err      -> emit (BranchError (T.pack err))
        Right mapping -> do
          notify env branch mapping
          emit (TicksInvalidated mid mapping)

    DeleteTick mid tickId_ -> do
      r <- runAction env (handleDeleteTick branch tickId_)
      case r of
        Left err      -> emit (BranchError (T.pack err))
        Right mapping -> do
          notify env branch mapping
          emit (TicksInvalidated mid mapping)

-- ---------------------------------------------------------------------------
-- Handlers
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
        void $ trackBranch @Source @Tracker filePairs
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

-- | Add an annotation note tick referencing @refTickId@ on branch @branch@.
--   The note tick is appended at head; the invariant (note comes after its ref)
--   is satisfied because the ref must already exist in history.
handleAddNote
  :: SessionEffects r
  => T.Text -> T.Text -> T.Text
  -> Sem r BranchTick
handleAddNote branch refTickIdTxt text =
  withBranch @Main branch $ do
    let refId = TickId refTickIdTxt
    -- Verify the ref exists in this branch's history.
    ticks <- follow @Main [] $ \acc t -> (t : acc, tickParent t)
    case filter (\t -> tickId t == refId) ticks of
      [] -> fail $ "ref tick not found in branch: " <> T.unpack refTickIdTxt
      _  -> do
        let headId = case ticks of { (h:_) -> Just (unTickId (tickId h)); [] -> Nothing }
        newId <- storeAs @Main (Note refId text)
        return $ BranchTickNote
          { btTickId = unTickId newId
          , btParent = headId
          , btRef    = refTickIdTxt
          , btText   = text
          }

-- | Move a tick to a new position in the chain.
--   Delegates to 'Storyteller.Edit.moveTick' which enforces ordering invariants
--   and performs the move as a single nested At.
handleMoveTick
  :: SessionEffects r
  => T.Text -> T.Text -> Maybe T.Text
  -> Sem r [(T.Text, T.Text)]
handleMoveTick branch tickIdTxt mAfterTickIdTxt =
  withBranch @Main branch $ do
    let tid    = TickId tickIdTxt
        mAfter = TickId <$> mAfterTickIdTxt
    mapping <- moveTick @Main tid mAfter
    return (toTextPairs mapping)

-- | Delete a tick from the chain. Any note ticks referencing it are left with
--   a dangling ref. Returns the old→new id mapping for the rebase.
handleDeleteTick
  :: SessionEffects r
  => T.Text -> T.Text
  -> Sem r [(T.Text, T.Text)]
handleDeleteTick branch tickIdTxt =
  withBranch @Main branch $ do
    mapping <- deleteTick @Main (TickId tickIdTxt)
    return (toTextPairs mapping)

-- ---------------------------------------------------------------------------
-- Snapshots — sent on connect
-- ---------------------------------------------------------------------------

snapshot :: ServerEnv -> T.Text -> IO (Either String [FilePath])
snapshot env branch = runAction env $ do
  let name = BranchName branch
  getBranch name >>= \case
    Nothing -> return []
    Just _  -> withBranch @Main branch $ do
      wt <- currentWorkingTree @Main
      return $ map fst (textFiles wt)

tickSnapshot :: ServerEnv -> T.Text -> IO (Either String [BranchTick])
tickSnapshot env branch = runAction env $ do
  getBranch (BranchName branch) >>= \case
    Nothing -> return []
    Just _  -> withBranch @Main branch $ do
      ticks <- follow @Main [] $ \acc tick -> (tick : acc, tickParent tick)
      -- Exclude the root tick (tickParent = Nothing) — it's an internal
      -- implementation detail, not a content tick visible to the client.
      let contentTicks = filter ((/= Nothing) . tickParent) ticks
      return $ map toBranchTick (reverse contentTicks)
  where
    toBranchTick tick =
      let tid = unTickId (tickId tick)
          par = unTickId <$> tickParent tick
          rs  = map unTickId (tickRefs (tickData tick))
      in case fromTick tick of
           Just (Note ref body) -> BranchTickNote tid par (unTickId ref) body
           Nothing              -> BranchTickAtom tid par rs (tickMessage (tickData tick))

-- ---------------------------------------------------------------------------
-- Pub/sub broadcast
-- ---------------------------------------------------------------------------

-- | Broadcast a branch-invalidated notification to all subscribed connections.
--   Called after any operation that mutates the tick chain.
notify :: ServerEnv -> T.Text -> [(T.Text, T.Text)] -> IO ()
notify env branch mapping =
  atomically $ writeTChan (envNotifyChan env) BranchNotification
    { bnBranch  = branch
    , bnMapping = mapping
    }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

toTextPairs :: [(TickId, TickId)] -> [(T.Text, T.Text)]
toTextPairs = map (\(o, n) -> (unTickId o, unTickId n))

textFiles :: WorkingTree -> [(FilePath, ObjectHash)]
textFiles wt = [ (path, hash) | (path, FSFile hash) <- Map.toList wt ]

currentWorkingTree
  :: forall branch r
  .  Members '[StoryBranch branch, Git, Fail] r
  => Sem r WorkingTree
currentWorkingTree = do
  ticks <- follow @branch [] $ \acc tick -> (tick : acc, tickParent tick)
  case ticks of
    [] -> return Map.empty
    _  -> loadWorkingTree (ObjectHash (unTickId (tickId (last ticks))))
