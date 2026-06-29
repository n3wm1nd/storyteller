{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Command dispatcher.
--
-- Dispatch is a pure router: each line names the handler, names what comes
-- back, and names the event it becomes. Handlers live below and are
-- responsible only for doing the work and returning a value — they never
-- emit events themselves.
module Server.Dispatch
  ( dispatch
  , snapshot
  ) where

import Control.Monad (void, forM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Aeson (encode)
import qualified Data.Yaml as Yaml
import qualified Network.WebSockets as WS
import Polysemy (Members, Sem)
import Polysemy.Error (throw)
import Polysemy.Fail (Fail)

import Server.Env (ServerEnv)
import Server.Protocol
import Server.Run (runAction, SessionEffects)
import Server.Util (withBranch, withBranchSplitter)

import Storyteller.Agent.Append (appendAgent)
import Storyteller.Agent.CharGen (charGenCommit, ScenarioTemplate(..), RngSeed(..))
import Storyteller.Agent.Tracker (trackBranch)
import Storyteller.Git (BranchTag(..), FSNode(..), WorkingTree, runBranchAndFS, loadWorkingTree)
import Storyteller.Storage (StoryBranch, createBranch, getBranch, follow)
import Storyteller.Types (BranchName(..), TickId(..), Tick(..))

import Runix.FileSystem (FileSystem, FileSystemRead, fileExists, readFile)
import Runix.Git (Git, ObjectHash(..), readBlob)

import Prelude hiding (readFile)

data Main
data Source
data Tracker
data CharBranch

-- ---------------------------------------------------------------------------
-- Dispatch
-- ---------------------------------------------------------------------------

dispatch :: ServerEnv -> T.Text -> WS.Connection -> Command -> IO ()
dispatch env branch conn cmd = do
  let emit = WS.sendTextData conn . encode
      orErr (Left err) _ = emit (Error (T.pack err))
      orErr (Right v)  f = emit (f v)
      orErrN (Left err) _ = emit (Error (T.pack err))
      orErrN (Right vs) f = mapM_ (emit . f) vs

  case cmd of
    Append mid path content -> do
      r <- runAction env (handleAppend branch path content)
      orErr r (FileUpdated mid path)

    Track mid source files -> do
      r <- runAction env (handleTrack branch source files)
      orErrN r (uncurry (FileUpdated mid))

    CharGen mid path scenario seed -> do
      r <- runAction env (handleCharGen branch path scenario seed)
      orErr r (FileUpdated mid path)

    ReadFile mid path -> do
      r <- runAction env (handleReadFile branch path)
      orErr r (FileContent mid path)

    DeleteFile _mid _path ->
      emit (Error "delete.file not yet implemented")

-- ---------------------------------------------------------------------------
-- Handlers — do the work, return a value, never emit
-- ---------------------------------------------------------------------------

-- | Append content to a file; return updated file content.
handleAppend
  :: SessionEffects r
  => T.Text -> FilePath -> T.Text -> Sem r T.Text
handleAppend branch path content =
  withBranchSplitter @Main branch $ do
    void $ appendAgent @(BranchTag Main) @Main path content
    decodeFile @(BranchTag Main) path

-- | Track atoms from source branch into target files; return (destPath, content) pairs.
handleTrack
  :: SessionEffects r
  => T.Text -> T.Text -> [TrackFile] -> Sem r [(FilePath, T.Text)]
handleTrack branch source files = do
  let target    = BranchName branch
      sourceName = BranchName source
      filePairs  = map (\f -> (trackFrom f, trackTo f)) files
      destPaths  = map snd filePairs
  getBranch target >>= \case
    Nothing -> void $ createBranch target
    Just _  -> return ()
  runBranchAndFS @Source sourceName
    $ runBranchAndFS @Tracker target $ do
        void $ trackBranch @Source @Tracker @(BranchTag Tracker) filePairs
        mapM (\p -> (p,) <$> decodeFile @(BranchTag Tracker) p) destPaths

-- | Generate a character sheet; return the written file content.
handleCharGen
  :: SessionEffects r
  => T.Text -> FilePath -> T.Text -> Maybe Int -> Sem r T.Text
handleCharGen branch path scenario seed = do
  let name = BranchName branch
  template <- case Yaml.decodeEither' (TE.encodeUtf8 scenario) of
    Left  err -> throw (Yaml.prettyPrintParseException err)
    Right val -> return (ScenarioTemplate val)
  getBranch name >>= \case
    Nothing -> void $ createBranch name
    Just _  -> return ()
  runBranchAndFS @CharBranch name $ do
    void $ charGenCommit @CharBranch template (RngSeed <$> seed) path
    decodeFile @(BranchTag CharBranch) path

-- | Read a file; return its content.
handleReadFile
  :: SessionEffects r
  => T.Text -> FilePath -> Sem r T.Text
handleReadFile branch path =
  withBranch @Main branch $ do
    fileExists @(BranchTag Main) path >>= \case
      False -> throw ("file not found: " <> path)
      True  -> decodeFile @(BranchTag Main) path

-- ---------------------------------------------------------------------------
-- Snapshot (used by Session on connect)
-- ---------------------------------------------------------------------------

snapshot :: ServerEnv -> T.Text -> IO (Either String (Map FilePath T.Text))
snapshot env branch = runAction env $ do
  let name = BranchName branch
  getBranch name >>= \case
    Nothing -> return Map.empty
    Just _  -> withBranch @Main branch $ do
      wt <- currentWorkingTree @Main
      fmap Map.fromList $ forM (textFiles wt) $ \(path, hash) ->
        (path,) . TE.decodeUtf8With TE.lenientDecode <$> readBlob hash

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

decodeFile
  :: forall tag r. Members '[FileSystem tag, FileSystemRead tag, Fail] r
  => FilePath -> Sem r T.Text
decodeFile path = TE.decodeUtf8With TE.lenientDecode <$> readFile @tag path

textFiles :: WorkingTree -> [(FilePath, ObjectHash)]
textFiles wt = [ (path, hash) | (path, FSFile hash) <- Map.toList wt ]

currentWorkingTree
  :: forall branch r
  .  Members '[StoryBranch branch, Git, Fail] r
  => Sem r WorkingTree
currentWorkingTree = do
  ticks <- follow @branch [] $ \acc tick -> (tick : acc, tickParent tick)
  case ticks of
    []    -> return Map.empty
    (t:_) -> loadWorkingTree (ObjectHash (unTickId (tickId t)))
