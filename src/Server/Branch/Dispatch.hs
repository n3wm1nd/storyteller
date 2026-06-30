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
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (encode)
import qualified Data.Yaml as Yaml
import qualified Network.WebSockets as WS
import Polysemy (Members, Sem)
import Polysemy.Error (throw)
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, listAllFiles)

import Server.Branch.Protocol
import Server.Env (ServerEnv(..))
import Server.Run (runAction, SessionEffects)
import Server.Util (withBranch, withBranchSplitter)

import Storyteller.Agent (Prompt(..), Instruction(..))
import Storyteller.Atom (Atom(..))
import Storyteller.Agent.CharGen (charGenCommit, ScenarioTemplate(..), RngSeed(..))
import Storyteller.Agent.Tracker (trackBranch)
import Storyteller.Agent.Write (writeAgent)
import Storyteller.Edit (deleteTick, moveTick)
import Storyteller.Git (BranchTag(..), runBranchAndFS)
import Storyteller.Storage (StoryBranch, StoryStorage, createBranch, getBranch, follow, get, storeAs)
import Storyteller.Types (BranchName(..), TickId(..), Tick(..), TickData(..), Note(..), TickType(..), tickId, tickParent, tickTypeOf)


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
  r <- runAction env $ case cmd of
    Track mid source files -> do
      ps <- handleTrack branch source files
      return $ map (FileAdded mid) ps

    CharGen mid path scenario seed -> do
      handleCharGen branch path scenario seed
      return [FileAdded mid path]

    ReadTicks _mid -> do
      ts <- handleReadTicks branch
      return [BranchTicks ts]

    AddNote _mid refTickId noteText_ -> do
      tick <- handleAddNote branch refTickId noteText_
      return [BranchTicks [tick]]

    MoveTick mid tickId_ mAfterTickId -> do
      mapping <- handleMoveTick branch tickId_ mAfterTickId
      return [TicksInvalidated mid mapping]

    DeleteTick mid tickId_ -> do
      mapping <- handleDeleteTick branch tickId_
      return [TicksInvalidated mid mapping]

    ChatPrompt _mid path promptText_ -> do
      handleChatPrompt branch path promptText_
      return []

  case r of
    Left err  -> emit (BranchError (T.pack err))
    Right evs -> mapM_ emit evs

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

handleChatPrompt
  :: SessionEffects r
  => T.Text -> FilePath -> T.Text
  -> Sem r ()
handleChatPrompt branch path promptText_ =
  withBranchSplitter @Main branch $ do
    storeAs @Main (Prompt path promptText_)
    writeAgent @(BranchTag Main) @Main path (Instruction promptText_) []
    return ()

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
    Just _  -> withBranch @Main branch $
      branchFiles @Main

handleReadTicks :: SessionEffects r => T.Text -> Sem r [BranchTick]
handleReadTicks branch =
  getBranch (BranchName branch) >>= \case
    Nothing -> return []
    Just _  -> withBranch @Main branch $ do
      ticks <- follow @Main [] $ \acc tick -> (tick : acc, tickParent tick)
      let contentTicks = filter ((/= Nothing) . tickParent) ticks
      return $ map tickToBranchTick (reverse contentTicks)

tickSnapshot :: ServerEnv -> T.Text -> IO (Either String [BranchTick])
tickSnapshot env branch = runAction env (handleReadTicks branch)

-- ---------------------------------------------------------------------------
-- Pub/sub broadcast
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

toTextPairs :: [(TickId, TickId)] -> [(T.Text, T.Text)]
toTextPairs = map (\(o, n) -> (unTickId o, unTickId n))

tickToBranchTick :: Tick -> BranchTick
tickToBranchTick tick =
  let tid = unTickId (tickId tick)
      par = unTickId <$> tickParent tick
      rs  = map unTickId (tickRefs (tickData tick))
  in case tickTypeOf tick of
       Just "note"   | Just (Note ref body)   <- fromTick tick
                     -> BranchTickNote   tid par (unTickId ref) body
       Just "prompt" | Just (Prompt file txt) <- fromTick tick
                     -> BranchTickPrompt tid par (T.pack file) txt
       Just "atom"   | Just atom <- fromTick tick
                     -> BranchTickAtom   tid par rs (atomMessage atom)
       _             -> BranchTickAtom   tid par rs (tickMessage (tickData tick))

branchFiles
  :: forall branch r
  .  Members '[FileSystem (BranchTag branch), Fail] r
  => Sem r [FilePath]
branchFiles = listAllFiles @(BranchTag branch) "/"
