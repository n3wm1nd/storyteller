{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Dispatch for /branch/{name}/{path} connections.
module Server.File.Dispatch
  ( dispatch
  , snapshot
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Aeson (encode)
import qualified Network.WebSockets as WS
import Polysemy (Members, Sem)
import Polysemy.Error (throw)
import Polysemy.Fail (Fail)

import Server.Env (ServerEnv)
import Server.File.Protocol
import Server.Run (runAction, SessionEffects)
import Server.Util (withBranchSplitter)

import Storyteller.Agent.Append (appendAgent)
import Storyteller.Git (BranchTag)
import Storyteller.Storage (StoryBranch, getBranch)
import Storyteller.Types (BranchName(..))

import Runix.FileSystem (FileSystemRead, FileSystemWrite, fileExists, readFile)

import Prelude hiding (readFile)

data Main

-- | Initial snapshot sent on connect: content if the file exists, absent if not.
snapshot :: ServerEnv -> T.Text -> FilePath -> IO (Either String (Maybe T.Text))
snapshot env branch path = runAction env $ do
  let name = BranchName branch
  getBranch name >>= \case
    Nothing -> return Nothing
    Just _  -> withBranchSplitter @Main branch $
      fileExists @(BranchTag Main) path >>= \case
        False -> return Nothing
        True  -> Just . TE.decodeUtf8With TE.lenientDecode
                    <$> readFile @(BranchTag Main) path

dispatch :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> FileCommand -> IO ()
dispatch env branch path conn cmd = do
  let emit = WS.sendTextData conn . encode
      orErr (Left err) _ = emit (FileError (T.pack err))
      orErr (Right v)  f = emit (f v)

  case cmd of
    Append mid content -> do
      r <- runAction env (handleAppend branch path content)
      orErr r (FileUpdated mid)

    Read mid -> do
      r <- runAction env (handleRead branch path)
      orErr r $ \case
        Nothing -> FileAbsent mid
        Just c  -> FileContent mid c

    Delete _mid ->
      emit (FileError "delete not yet implemented")

-- ---------------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------------

handleAppend :: SessionEffects r => T.Text -> FilePath -> T.Text -> Sem r T.Text
handleAppend branch path content =
  withBranchSplitter @Main branch $ do
    _ <- appendAgent @(BranchTag Main) @Main path content
    TE.decodeUtf8With TE.lenientDecode <$> readFile @(BranchTag Main) path

handleRead :: SessionEffects r => T.Text -> FilePath -> Sem r (Maybe T.Text)
handleRead branch path =
  withBranchSplitter @Main branch $
    fileExists @(BranchTag Main) path >>= \case
      False -> return Nothing
      True  -> Just . TE.decodeUtf8With TE.lenientDecode
                  <$> readFile @(BranchTag Main) path
