{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Aeson (encode)
import qualified Network.WebSockets as WS
import Polysemy (Sem, Members)
import Polysemy.Fail (Fail)

import Server.Env (ServerEnv)
import Server.File.Protocol (FileCommand(..), FileEvent(..))
import qualified Server.File.Protocol as Protocol
import Server.Run (runAction, SessionEffects)
import Server.Util (withBranch, withBranchSplitter)

import Storyteller.Agent.Append (appendAgent)
import Storyteller.Runtime (Main)
import Storyteller.Storage (StoryBranch, fileTicks, getBranch)
import qualified Storyteller.Storage as Storage
import Storyteller.Edit (deleteTick, editAtom, moveTick)
import Storyteller.Types (BranchName(..), TickId(..))

import Prelude hiding (readFile, writeFile)

-- ---------------------------------------------------------------------------
-- Snapshot + dispatch
-- ---------------------------------------------------------------------------

snapshot :: ServerEnv -> T.Text -> FilePath -> IO (Either String (Maybe [Protocol.FileTick]))
snapshot env branch path = runAction env $ queryFileTicks branch path

dispatch :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> FileCommand -> IO ()
dispatch env branch path conn cmd = do
  let emit = WS.sendTextData conn . encode
  case cmd of
    Append _mid content -> do
      r <- runAction env (handleAppend branch path content)
      case r of
        Left err   -> emit (FileError (T.pack err))
        Right tick -> emit (TickAppended tick)

    Read _mid -> do
      r <- runAction env (queryFileTicks branch path)
      case r of
        Left err           -> emit (FileError (T.pack err))
        Right Nothing      -> emit (FileAbsent Nothing)
        Right (Just ticks) -> emit (FileTicks ticks)

    Delete _mid ->
      emit (FileError "file delete not yet implemented")

    EditAtom mid tickIdTxt newContent -> do
      r <- runAction env (handleEditAtom branch path tickIdTxt newContent)
      case r of
        Left err            -> emit (FileError (T.pack err))
        Right (oldId, tick) -> emit (AtomReplaced mid oldId tick)

    DeleteAtom mid tickIdTxt -> do
      r <- runAction env (handleDeleteAtom branch path tickIdTxt)
      case r of
        Left err      -> emit (FileError (T.pack err))
        Right mapping -> emit (AtomDeleted mid tickIdTxt mapping)

    MoveAtom mid tickIdTxt mAfterTickId -> do
      r <- runAction env (handleMoveAtom branch path tickIdTxt mAfterTickId)
      case r of
        Left err      -> emit (FileError (T.pack err))
        Right mapping -> emit (AtomMoved mid mapping)

-- ---------------------------------------------------------------------------
-- Query helpers
-- ---------------------------------------------------------------------------

-- | Fetch file ticks via the StoryBranch effect.
--   Returns Nothing if the branch doesn't exist, Just [] if the file is absent.
queryFileTicks
  :: SessionEffects r
  => T.Text -> FilePath -> Sem r (Maybe [Protocol.FileTick])
queryFileTicks branch path =
  getBranch (BranchName branch) >>= \case
    Nothing -> return Nothing
    Just _  -> withBranch @Main branch $
      fmap (Just . map toProtocolTick) (fileTicks @Main path)

toProtocolTick :: Storage.FileTick -> Protocol.FileTick
toProtocolTick ft = Protocol.FileTick
  { Protocol.ftTickId  = Storage.ftTickId  ft
  , Protocol.ftKind    = Storage.ftKind    ft
  , Protocol.ftRefs    = Storage.ftRefs    ft
  , Protocol.ftFields  = Storage.ftFields  ft
  , Protocol.ftMessage = Storage.ftMessage ft
  , Protocol.ftContent = Storage.ftContent ft
  , Protocol.ftParent  = Storage.ftParent  ft
  }

-- ---------------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------------

handleAppend :: SessionEffects r => T.Text -> FilePath -> T.Text -> Sem r Protocol.FileTick
handleAppend branch path content =
  withBranchSplitter @Main branch $ do
    _tids <- appendAgent @Main path content
    headTick path

handleEditAtom
  :: SessionEffects r
  => T.Text -> FilePath -> T.Text -> T.Text
  -> Sem r (T.Text, Protocol.FileTick)
handleEditAtom branch path tickIdTxt newContent =
  withBranch @Main branch $ do
    (_newTid, _mapping) <- editAtom @Main
      (TickId tickIdTxt) path (TE.encodeUtf8 newContent)
    tick <- headTick path
    return (tickIdTxt, tick)

handleDeleteAtom
  :: SessionEffects r
  => T.Text -> FilePath -> T.Text
  -> Sem r [(T.Text, T.Text)]
handleDeleteAtom branch _path tickIdTxt =
  withBranch @Main branch $ do
    mapping <- deleteTick @Main (TickId tickIdTxt)
    return [ (unTickId o, unTickId n) | (o, n) <- mapping ]

handleMoveAtom
  :: SessionEffects r
  => T.Text -> FilePath -> T.Text -> Maybe T.Text
  -> Sem r [(T.Text, T.Text)]
handleMoveAtom branch _path tickIdTxt mAfterTickId =
  withBranch @Main branch $ do
    mapping <- moveTick @Main (TickId tickIdTxt) (TickId <$> mAfterTickId)
    return [ (unTickId o, unTickId n) | (o, n) <- mapping ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Return the head (most recent) tick for @path@. Must be called inside
--   a 'withBranch @Main' / 'withBranchSplitter @Main' context.
headTick
  :: Members '[StoryBranch Main, Fail] r
  => FilePath -> Sem r Protocol.FileTick
headTick path = do
  tks <- fileTicks @Main path
  case reverse tks of
    []     -> fail "no ticks at HEAD for this file"
    (ft:_) -> return (toProtocolTick ft)
