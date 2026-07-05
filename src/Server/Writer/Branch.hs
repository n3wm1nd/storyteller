{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Storyteller.Core.Edit (commitFiles)
import Storyteller.Core.Git (BranchTag, runBranchAndFS, withStorage)
import Storyteller.Core.Storage (createBranch, getBranch)
import Storyteller.Core.Types (BranchName(..))
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
  runBranchAndFS @CharBranch name $ do
    rngSeed <- maybe drawSeed return (RngSeed <$> seed)
    let sheet = charGenAgent template rngSeed
    writeFile @(BranchTag CharBranch) path (TE.encodeUtf8 (unSheet sheet))
    void $ commitFiles @(BranchTag CharBranch) @CharBranch [path]

-- | Write one or more files' content directly into the branch, bypassing
--   the chat-agent pipeline entirely (an upload isn't an LLM-authored
--   append, it's raw bytes the user already chose). Runs on the branch
--   scope already open for the caller ('BranchOpen') — the sole caller is
--   'uploadFile' below, which opens that scope itself for the HTTP PUT
--   endpoint's one-shot case.
--
--   Each path is reconciled independently via 'commitFiles' — same
--   new-file-vs-edit-existing-file logic 'commitWorkingTree' uses, just
--   scoped to the uploaded paths instead of the whole branch, so an upload
--   never touches any other file's pending working-tree state.
--
--   Returns the uploaded paths, so the caller can push 'FileAdded' events.
uploadFiles
  :: BranchOpen r
  => [(FilePath, BS.ByteString)] -- ^ (path, content) pairs
  -> Sem r [FilePath]
uploadFiles files = do
  mapM_ (\(path, content) -> writeFile @(BranchTag Main) path content) files
  let paths = map fst files
  _ <- commitFiles @(BranchTag Main) @Main paths
  return paths

-- | One-shot variant of 'uploadFiles' for the HTTP @PUT /branch/{name}/{path}@
--   endpoint, which has no already-open 'BranchOpen' connection to run
--   against. Opens the branch's scope itself and wraps the write in its own
--   'withStorage' transaction, same as a WS command's per-command 'handle'
--   does (see 'Server.Writer.File.Connection'); that transaction's ref update
--   is what 'gitNotify'/'storageNotify' (wired into every
--   'runAction'/'wsAction' call, including this endpoint's) pick up to notify
--   any connections watching this branch — nothing extra needed here to
--   "poke" them.
uploadFile :: SessionEffects r => T.Text -> FilePath -> BS.ByteString -> Sem r ()
uploadFile branch path content =
  withStorage (withBranch @Main branch (void (uploadFiles [(path, content)])))
