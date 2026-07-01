{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Branch-level business logic.
--
-- Pure (or limited-effect) functions that implement branch operations.
-- No JSON, no WebSocket, no T.Text ids — callers handle the boundary.
-- These functions are the unit under test.
module Server.Branch
  ( branchState
  , branchStateSince
  , addNote
  , moveTickInBranch
  , deleteTickFromBranch
  , trackFiles
  , charGen
  , chatPrompt
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy (Members, Sem)
import Polysemy.Error (throw)
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, listAllFiles)
import Runix.Logging (info)

import Polysemy.Error (Error, throw)
import Runix.Git (Git)

import Server.Protocol (Update(..), tickToWireTick)
import Server.Run (SessionEffects)
import Server.Util (withBranch, withBranchSplitter)
import Storyteller.Storage (StoryStorage)

import Storyteller.Agent (Prompt(..), Instruction(..))
import Storyteller.Agent.CharGen (charGenCommit, ScenarioTemplate(..), RngSeed(..))
import Storyteller.Agent.Tracker (trackBranch)
import Storyteller.Agent.Write (writeAgent)
import Storyteller.Edit (deleteTick, moveTick)
import Storyteller.Git (BranchTag(..), runBranchAndFS)
import Storyteller.Storage (StoryBranch, createBranch, getBranch, follow, get, storeAs)
import Storyteller.Types (BranchName(..), TickId(..), Note(..), tickId, tickParent, unTickId)
import qualified Data.Yaml as Yaml

data Main
data Source
data Tracker
data CharBranch

-- ---------------------------------------------------------------------------
-- State query
-- ---------------------------------------------------------------------------

-- | Full branch state: all ticks and current HEAD id.
--   Called on connect and after every mutation.
branchState :: Members '[StoryStorage, Git, Error String, Fail] r => BranchName -> Sem r (Maybe ([FilePath], Update))
branchState name = branchStateSince name Nothing

-- | Branch state, optionally incremental. When 'since' names a tick still
--   reachable from HEAD, only ticks newer than it are included — the common
--   case for keeping an already-caught-up connection informed of new writes.
--   When 'since' is 'Nothing', or no longer reachable (e.g. a move/replace
--   rewrote history out from under it), the walk runs all the way to root
--   and the full chain is returned — the correct, if pricier, fallback.
branchStateSince
  :: Members '[StoryStorage, Git, Error String, Fail] r
  => BranchName -> Maybe TickId -> Sem r (Maybe ([FilePath], Update))
branchStateSince name@(BranchName n) since =
  getBranch name >>= \case
    Nothing -> return Nothing
    Just _  -> withBranch @Main n $ do
      files  <- listAllFiles @(BranchTag Main) "/"
      ticks  <- follow @Main [] $ \acc t ->
        if Just (tickId t) == since
          then (acc, Nothing)
          else (t : acc, tickParent t)
      headTk <- get @Main
      let upd = Update (map tickToWireTick ticks) (unTickId (tickId headTk))
      return (Just (files, upd))

-- ---------------------------------------------------------------------------
-- Mutations — each returns Unit; caller fetches updated state via branchState
-- ---------------------------------------------------------------------------

-- | Add an annotation note referencing an existing tick.
addNote :: Members '[StoryStorage, Git, Error String, Fail] r => BranchName -> TickId -> T.Text -> Sem r ()
addNote (BranchName n) refId text =
  withBranch @Main n $ do
    ticks <- follow @Main [] $ \acc t -> (t : acc, tickParent t)
    case filter (\t -> tickId t == refId) ticks of
      [] -> fail $ "ref tick not found: " <> T.unpack (unTickId refId)
      _  -> void $ storeAs @Main (Note refId text)

-- | Move a tick to a new position in the chain.
moveTickInBranch :: Members '[StoryStorage, Git, Error String, Fail] r => BranchName -> TickId -> Maybe TickId -> Sem r ()
moveTickInBranch (BranchName n) tid mAfter =
  withBranch @Main n $
    void $ moveTick @Main tid mAfter

-- | Delete a tick from the chain.
deleteTickFromBranch :: Members '[StoryStorage, Git, Error String, Fail] r => BranchName -> TickId -> Sem r ()
deleteTickFromBranch (BranchName n) tid =
  withBranch @Main n $
    void $ deleteTick @Main tid

-- | Track files from a source branch into a target branch.
--   Creates the target branch if it doesn't exist.
--   Returns the destination paths of tracked files.
trackFiles
  :: SessionEffects r
  => BranchName           -- ^ target branch
  -> BranchName           -- ^ source branch
  -> [(FilePath, FilePath)] -- ^ (from, to) pairs
  -> Sem r [FilePath]
trackFiles target source pairs = do
  getBranch target >>= \case
    Nothing -> void $ createBranch target
    Just _  -> return ()
  let destPaths = map snd pairs
  runBranchAndFS @Source source
    $ runBranchAndFS @Tracker target $ do
        mapM_ (trackBranch @Source @Tracker) pairs
        return destPaths

-- | Run chargen and commit the result to a branch.
--   Creates the branch if it doesn't exist.
charGen
  :: SessionEffects r
  => BranchName
  -> FilePath
  -> T.Text     -- ^ YAML scenario
  -> Maybe Int  -- ^ RNG seed
  -> Sem r ()
charGen name path scenario seed = do
  template <- case Yaml.decodeEither' (TE.encodeUtf8 scenario) of
    Left  err -> throw (Yaml.prettyPrintParseException err)
    Right val -> return (ScenarioTemplate val)
  getBranch name >>= \case
    Nothing -> void $ createBranch name
    Just _  -> return ()
  runBranchAndFS @CharBranch name $
    void $ charGenCommit @CharBranch template (RngSeed <$> seed) path

-- | Store a prompt tick then run the write agent.
chatPrompt :: SessionEffects r => BranchName -> FilePath -> T.Text -> Sem r ()
chatPrompt (BranchName n) path prompt =
  withBranchSplitter @Main n $ do
    _ <- storeAs @Main (Prompt path prompt)
    info $ "writer agent starting: " <> T.pack path
    _ <- writeAgent @(BranchTag Main) @Main path (Instruction prompt) []
    info $ "writer agent done: " <> T.pack path

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

