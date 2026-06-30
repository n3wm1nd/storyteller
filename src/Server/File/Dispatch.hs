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
import Server.File.Protocol
import Server.Run (runAction, SessionEffects)
import Server.Util (withBranch, withBranchSplitter)

import Storyteller.Agent.Append (appendAgent)
import Storyteller.Runtime (Main)
import Storyteller.Storage (AtomEntry(..), StoryBranch, fileAtoms, getBranch)
import Storyteller.Edit (deleteTick, editAtom)
import Storyteller.Types (BranchName(..), TickId(..))

import Prelude hiding (readFile, writeFile)

-- ---------------------------------------------------------------------------
-- Snapshot + dispatch
-- ---------------------------------------------------------------------------

snapshot :: ServerEnv -> T.Text -> FilePath -> IO (Either String (Maybe [FileAtom]))
snapshot env branch path = runAction env $ queryFileAtoms branch path

dispatch :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> FileCommand -> IO ()
dispatch env branch path conn cmd = do
  let emit = WS.sendTextData conn . encode
  case cmd of
    Append _mid content -> do
      r <- runAction env (handleAppend branch path content)
      case r of
        Left err   -> emit (FileError (T.pack err))
        Right atom -> emit (AtomAppended atom)

    Read _mid -> do
      r <- runAction env (queryFileAtoms branch path)
      case r of
        Left err           -> emit (FileError (T.pack err))
        Right Nothing      -> emit (FileAbsent Nothing)
        Right (Just atoms) -> emit (FileAtoms atoms)

    Delete _mid ->
      emit (FileError "file delete not yet implemented")

    EditAtom mid tickIdTxt newContent -> do
      r <- runAction env (handleEditAtom branch path tickIdTxt newContent)
      case r of
        Left err            -> emit (FileError (T.pack err))
        Right (oldId, atom) -> emit (AtomReplaced mid oldId atom)

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

-- | Fetch file atoms via the StoryBranch effect.
--   Returns Nothing if the branch doesn't exist, Just [] if the file is absent.
queryFileAtoms
  :: SessionEffects r
  => T.Text -> FilePath -> Sem r (Maybe [FileAtom])
queryFileAtoms branch path =
  getBranch (BranchName branch) >>= \case
    Nothing -> return Nothing
    Just _  -> withBranch @Main branch $
      fmap (Just . map toFileAtom) (fileAtoms @Main path)

toFileAtom :: AtomEntry -> FileAtom
toFileAtom ae = FileAtom
  { atomTickId  = aeTickId  ae
  , atomContent = aeContent ae
  , atomMessage = aeMessage ae
  , atomParent  = aeParent  ae
  }

-- ---------------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------------

handleAppend :: SessionEffects r => T.Text -> FilePath -> T.Text -> Sem r FileAtom
handleAppend branch path content =
  withBranchSplitter @Main branch $ do
    _tids <- appendAgent @Main path content
    headAtom path

handleEditAtom
  :: SessionEffects r
  => T.Text -> FilePath -> T.Text -> T.Text
  -> Sem r (T.Text, FileAtom)
handleEditAtom branch path tickIdTxt newContent =
  withBranch @Main branch $ do
    (_newTid, _mapping) <- editAtom @Main
      (TickId tickIdTxt) path (TE.encodeUtf8 newContent)
    atom <- headAtom path
    return (tickIdTxt, atom)

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
handleMoveAtom _branch _path _tickIdTxt _mAfterTickId =
  fail "move not yet implemented"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Return the head (most recent) atom for @path@. Must be called inside
--   a 'withBranch @Main' / 'withBranchSplitter @Main' context.
headAtom
  :: Members '[StoryBranch Main, Fail] r
  => FilePath -> Sem r FileAtom
headAtom path = do
  atoms <- fileAtoms @Main path
  case reverse atoms of
    []     -> fail "no atoms at HEAD for this file"
    (ae:_) -> return (toFileAtom ae)
