{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Branch-level business logic.
--
-- Most of these functions assume the branch's storage/filesystem scope
-- ('BranchOpen') is already live in the ambient stack — a branch connection
-- enters it once, when the connection starts, not per command. Only
-- 'trackFiles' and 'charGen' open their own (additional) branch scopes on
-- top of that, because the branch they need (the source to track from, or a
-- brand-new target) is only known from the command payload at dispatch time.
--
-- No JSON, no WebSocket, no T.Text ids — callers handle the boundary.
-- These functions are the unit under test.
module Server.Branch
  ( Main
  , BranchOpen
  , branchState
  , branchStateSince
  , addNote
  , moveTickInBranch
  , deleteTickFromBranch
  , trackFiles
  , charGen
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy (Members, Sem)
import Polysemy.Error (throw)
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, listAllFiles)

import Server.Protocol (Update(..), tickToWireTick)
import Server.Run (SessionEffects)

import Storyteller.Agent.CharGen (charGenCommit, ScenarioTemplate(..), RngSeed(..))
import Storyteller.Agent.Tracker (trackBranch)
import Storyteller.Edit (deleteTick, moveTick)
import Storyteller.Git (BranchTag(..), runBranchAndFS)
import Storyteller.Storage (StoryBranch, StoryStorage, createBranch, getBranch, follow, reset, storeAs)
import Storyteller.Types (BranchName(..), TickId(..), Note(..), tickId, tickParent, unTickId)
import qualified Data.Yaml as Yaml

data Main
data Source
data Tracker
data CharBranch

-- | The effects live once a branch connection has entered its branch's
--   scope — one 'StoryBranch'/filesystem instance for the connection's
--   whole lifetime, not reopened per command.
type BranchOpen r =
  Members '[ StoryBranch Main
           , StoryStorage
           , FileSystemWrite (BranchTag Main)
           , FileSystemRead  (BranchTag Main)
           , FileSystem      (BranchTag Main)
           , Fail
           ] r

-- ---------------------------------------------------------------------------
-- State query
-- ---------------------------------------------------------------------------

-- | Full branch state: all ticks and current HEAD id.
branchState :: BranchOpen r => Sem r ([FilePath], Update)
branchState = branchStateSince Nothing

-- | Branch state, optionally incremental. When 'since' names a tick still
--   reachable from HEAD, only ticks newer than it are included — the common
--   case for keeping an already-caught-up connection informed of new writes.
--   When 'since' is 'Nothing', or no longer reachable (e.g. a move/replace
--   rewrote history out from under it), the walk runs all the way to root
--   and the full chain is returned — the correct, if pricier, fallback.
--
--   HEAD is derived from this same walk (its last, most-recent element)
--   rather than a separate 'get' — a second, independent HEAD resolution
--   could race a concurrent rebase and return a different, incompatible
--   position than the one 'ticks' was just walked from, sending a HEAD the
--   client can't fully resolve against the ticks in the very same update.
--   One walk, one resolution: whatever HEAD was at the moment 'follow'
--   resolved it, that's what both 'ticks' and the reported head describe.
--
--   'reset' first: 'listAllFiles' reads the in-memory working tree, which
--   this connection's long-lived stack loaded once at scope-entry and never
--   otherwise refreshes. Ticks/HEAD are read straight from git and are
--   always current regardless — only the file list needs this to see
--   writes made by other connections since we last synced.
branchStateSince :: BranchOpen r => Maybe TickId -> Sem r ([FilePath], Update)
branchStateSince since = do
  reset @Main
  files <- listAllFiles @(BranchTag Main) "/"
  ticks <- follow @Main [] $ \acc t ->
    if Just (tickId t) == since
      then (acc, Nothing)
      else (t : acc, tickParent t)
  case (reverse ticks, since) of
    (headTk : _, _) ->
      return (files, Update (map tickToWireTick ticks) (unTickId (tickId headTk)))
    ([], Just s) ->
      -- Nothing past 'since': HEAD hasn't moved from what the caller
      -- already has, no need to resolve it again.
      return (files, Update [] (unTickId s))
    ([], Nothing) ->
      fail "branchStateSince: branch has no ticks"

-- ---------------------------------------------------------------------------
-- Mutations on the already-open branch
-- ---------------------------------------------------------------------------

-- | Add an annotation note referencing an existing tick.
addNote :: BranchOpen r => TickId -> T.Text -> Sem r ()
addNote refId text = do
  ticks <- follow @Main [] $ \acc t -> (t : acc, tickParent t)
  case filter (\t -> tickId t == refId) ticks of
    [] -> fail $ "ref tick not found: " <> T.unpack (unTickId refId)
    _  -> void $ storeAs @Main (Note refId text)

-- | Move a tick to a new position in the chain.
moveTickInBranch :: BranchOpen r => TickId -> Maybe TickId -> Sem r ()
moveTickInBranch tid mAfter = void $ moveTick @Main tid mAfter

-- | Delete a tick from the chain.
deleteTickFromBranch :: BranchOpen r => TickId -> Sem r ()
deleteTickFromBranch tid = void $ deleteTick @Main tid

-- ---------------------------------------------------------------------------
-- Operations that open their own (additional) branch scopes
-- ---------------------------------------------------------------------------

-- | Track files from a source branch into a target branch.
--   Creates the target branch if it doesn't exist.
--   Returns the destination paths of tracked files.
--
--   Opens its own 'Source'/'Tracker'-tagged scopes rather than using the
--   ambient 'BranchOpen' one: the source branch is a different branch
--   entirely, known only from the command payload, and the target may not
--   exist yet (so it can't reuse a scope that assumes the branch is already
--   open and unchanging).
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
--
--   Opens its own 'CharBranch'-tagged scope for the same reason as
--   'trackFiles': the target branch may need to be created first.
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
