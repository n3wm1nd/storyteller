{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Branch-level business logic specific to the Writer application:
-- tracking files across branches (chapters, entity/tracker branches) and
-- resolving character-generation scenarios. Same shape as
-- 'Server.Core.Branch' — plain 'Sem' functions, no JSON/WebSocket — just too
-- specific to a writing workflow to live in the generic library.
module Server.Writer.Branch
  ( trackFiles
  , charGen
  , uploadFiles
  , uploadFile
  , saveFile
  , onlyWhilePresent
  ) where

import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy (Sem)
import Polysemy.Error (throw)
import Runix.FileSystem (writeFile)

import Server.Core.Branch (Main, BranchOpen)
import Server.Core.Run (SessionEffects)
import Server.Core.Util (withBranch)

import Storyteller.Writer.Agent.CharGen (charGenAgent, drawSeed, unSheet, ScenarioTemplate(..), RngSeed(..))
import Storyteller.Writer.Agent.Tracker (trackBranch)
import Storyteller.Writer.Presence (presentAt)
import Storyteller.Writer.Types (Character(..))
import Storyteller.Core.Atom (Atom(..))
import Storyteller.Core.Git (BranchTag, runBranchAndFS, runStorage, withStorage)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Core.Storage (createBranch, getBranch)
import Storyteller.Core.Types (BranchName(..), Tick, fromTick, tickId)
import qualified Data.Yaml as Yaml

import Prelude hiding (writeFile)

data Source
data Tracker
data CharBranch

-- | Track files from a source branch into a target branch.
--   Creates the target branch if it doesn't exist.
--   Returns the destination paths of tracked files.
--
--   Opens its own 'Source'/'Tracker'-tagged scopes rather than an ambient
--   one: the source branch is a different branch entirely, known only from
--   the command payload, and the target may not exist yet (so it can't
--   reuse a scope that assumes the branch is already open and unchanging).
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
        mapM_ (trackBranch @Source @Tracker (onlyWhilePresent (Character target))) pairs
        return destPaths

-- | Only copy an atom into @character@'s branch if that character was
--   marked present (see 'Storyteller.Writer.Presence.presentAt') on the
--   atom's own file at the point it was written -- narrative events that
--   happened while the character wasn't in the scene don't belong in their
--   journal. Drops every non-atom tick outright too (presence ticks, notes,
--   ...) -- a character's journal is a copy of narrative content, not a
--   mirror of every tick kind the source branch happens to record. A
--   dropped tick is never marked synced (see
--   'Storyteller.Writer.Agent.Tracker.dropUntilAfterLastSynced'), so a
--   later sync pass simply reconsiders it -- harmless, since presence at a
--   fixed historical position never changes, so it drops again every time.
onlyWhilePresent :: Core.StoreM m => Character -> Tick -> Core.StoreT m (Maybe Tick)
onlyWhilePresent character tick = case fromTick @Atom tick of
  Nothing -> pure Nothing
  Just (Atom file _) -> do
    present <- presentAt (tickId tick) file character
    pure (if present then Just tick else Nothing)

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
  runBranchAndFS @CharBranch name $ do
    rngSeed <- maybe drawSeed return (RngSeed <$> seed)
    let sheet = charGenAgent template rngSeed
    writeFile @(BranchTag CharBranch) path (TE.encodeUtf8 (unSheet sheet))
    void $ runStorage @CharBranch (Ops.commitFiles [path])

-- | Write one or more files' content directly into the branch, bypassing
--   the chat-agent pipeline entirely (an upload isn't an LLM-authored
--   append, it's raw bytes the user already chose). Runs on the branch
--   scope already open for the caller ('BranchOpen') — the sole caller is
--   'uploadFile' below, which opens that scope itself for the HTTP PUT
--   endpoint's one-shot case.
--
--   Every uploaded path lands as a 'Ops.addBinary' asset — an opaque,
--   path-aware 'Storage.Core.Binary' tick — regardless of whether its
--   content happens to decode as UTF-8. An upload is a deposit, not a
--   claim that the bytes are prose; deciding that is a separate, deliberate
--   "ingest this file" action (not yet built) that promotes a specific
--   path to atom-tracked text on request, not something this function
--   should guess at from the bytes alone.
--
--   Returns the uploaded paths, so the caller can push 'FileAdded' events.
uploadFiles
  :: BranchOpen r
  => [(FilePath, BS.ByteString)] -- ^ (path, content) pairs
  -> Sem r [FilePath]
uploadFiles files = do
  mapM_ (\(path, content) -> runStorage @Main (Ops.addBinary path content)) files
  return (map fst files)

-- | One-shot variant of 'uploadFiles' for the HTTP @PUT /branch/{name}/{path}@
--   endpoint, which has no already-open 'BranchOpen' connection to run
--   against. Opens the branch's scope itself and wraps the write in its own
--   'withStorage' transaction, same as a WS command's per-command 'handle'
--   does (see 'Server.Writer.File.Connection'); that transaction's ref update
--   is what 'Server.Writer.GitWorker'/'notifyRemaps' (wired into every
--   'runAction'/'wsAction' call, including this endpoint's) pick up to notify
--   any connections watching this branch — nothing extra needed here to
--   "poke" them.
uploadFile :: SessionEffects r => T.Text -> FilePath -> BS.ByteString -> Sem r ()
uploadFile branch path content =
  withStorage (withBranch @Main branch (void (uploadFiles [(path, content)])))

-- | One-shot save for the HTTP @PUT /branch/{name}/$raw/{path}@ endpoint —
--   the raw-edit-mode counterpart to 'uploadFile'. Where an upload deposits
--   an opaque 'Ops.addBinary' asset, this reconciles @content@ against the
--   path's existing atom chain via 'Ops.saveFile': a whole-file overwrite
--   from an editor that hands back the entire text still keeps whatever
--   atoms didn't actually change, same as any other reconciliation path
--   (chat regen, upload-then-ingest). Same one-shot branch-scope-opening
--   shape as 'uploadFile', for the same reason (no already-open
--   'BranchOpen' connection for this endpoint to run against).
saveFile :: SessionEffects r => T.Text -> FilePath -> T.Text -> Sem r ()
saveFile branch path content =
  withStorage (withBranch @Main branch (void (runStorage @Main (Ops.saveFile path content))))
