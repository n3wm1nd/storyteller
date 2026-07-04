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
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy (Sem)
import Polysemy.Error (throw)
import Runix.FileSystem (writeFile)

import Server.Core.Branch (Main, BranchOpen)
import Server.Core.Run (SessionEffects)

import Storyteller.Writer.Agent.CharGen (charGenAgent, drawSeed, unSheet, ScenarioTemplate(..), RngSeed(..))
import Storyteller.Writer.Agent.Tracker (trackBranch)
import Storyteller.Core.Edit (commitFiles)
import Storyteller.Core.Git (BranchTag, runBranchAndFS)
import Storyteller.Core.Storage (createBranch, getBranch, store)
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
    void $ store @CharBranch "character sheet"

-- | Write one or more dropped files' content directly into the branch,
--   bypassing the chat-agent pipeline (see TODO.md's Upload/download work
--   packet — a drag-and-drop upload isn't an LLM-authored append, it's raw
--   bytes the user already chose). Runs on the branch scope already open for
--   this connection ('BranchOpen'), unlike 'trackFiles'/'charGen': an upload
--   always targets the branch the client is connected to, never a separate
--   one that might still need creating.
--
--   Each path is reconciled independently via 'commitFiles' — same
--   new-file-vs-edit-existing-file logic 'commitWorkingTree' uses, just
--   scoped to the uploaded paths instead of the whole branch, so an upload
--   never touches any other file's pending working-tree state.
--
--   Returns the uploaded paths, so the caller can push 'FileAdded' events.
uploadFiles
  :: BranchOpen r
  => [(FilePath, T.Text)] -- ^ (path, content) pairs
  -> Sem r [FilePath]
uploadFiles files = do
  mapM_ (\(path, content) -> writeFile @(BranchTag Main) path (TE.encodeUtf8 content)) files
  let paths = map fst files
  _ <- commitFiles @(BranchTag Main) @Main paths
  return paths
