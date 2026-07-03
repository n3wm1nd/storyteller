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
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy (Sem)
import Polysemy.Error (throw)

import Server.Core.Run (SessionEffects)

import Storyteller.Writer.Agent.CharGen (charGenCommit, ScenarioTemplate(..), RngSeed(..))
import Storyteller.Writer.Agent.Tracker (trackBranch)
import Storyteller.Core.Git (runBranchAndFS)
import Storyteller.Core.Storage (createBranch, getBranch)
import Storyteller.Core.Types (BranchName(..))
import qualified Data.Yaml as Yaml

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
  runBranchAndFS @CharBranch name $
    void $ charGenCommit @CharBranch template (RngSeed <$> seed) path
