{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Dispatch for /branch/{name} connections.
--
-- Handles branch-level operations: file tree snapshot on connect, track, chargen.
-- Per-file operations (append, read, delete) live in Server.File.Dispatch.
module Server.Branch.Dispatch
  ( dispatch
  , snapshot
  ) where

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
import Server.Env (ServerEnv)
import Server.Run (runAction, SessionEffects)
import Server.Util (withBranch)

import Storyteller.Agent.CharGen (charGenCommit, ScenarioTemplate(..), RngSeed(..))
import Storyteller.Agent.Tracker (trackBranch)
import Storyteller.Git (BranchTag(..), WorkingTree, FSNode(..), runBranchAndFS, loadWorkingTree)
import Storyteller.Storage (StoryBranch, createBranch, getBranch, follow)
import Storyteller.Types (BranchName(..), TickId(..), Tick(..))

import Runix.Git (Git, ObjectHash(..))

data Main
data Source
data Tracker
data CharBranch

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
        void $ trackBranch @Source @Tracker @(BranchTag Tracker) filePairs
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

-- ---------------------------------------------------------------------------
-- Snapshot — file tree only, sent on connect
-- ---------------------------------------------------------------------------

snapshot :: ServerEnv -> T.Text -> IO (Either String [FilePath])
snapshot env branch = runAction env $ do
  let name = BranchName branch
  getBranch name >>= \case
    Nothing -> return []
    Just _  -> withBranch @Main branch $ do
      wt <- currentWorkingTree @Main
      return $ map fst (textFiles wt)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

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
