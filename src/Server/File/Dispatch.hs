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

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Aeson (encode)
import qualified Network.WebSockets as WS
import Polysemy (Sem)

import Server.Branch.Dispatch (notify)
import Server.Env (ServerEnv)
import Server.File.Protocol
import Server.Run (runAction, SessionEffects)
import Server.Util (withBranch, withBranchSplitter)

import Storyteller.Agent.Append (appendAgent)
import Storyteller.Git (BranchTag)
import Storyteller.Runtime (Main)
import Storyteller.Storage (getBranch)
import Storyteller.Edit (deleteTick, editAtom)
import Storyteller.Types (BranchName(..), TickId(..))

import Runix.Git
  ( ObjectHash(..), CommitData(..)
  , readBlob, resolveRef, readCommit, lookupPath
  , RefName(..)
  )

import Prelude hiding (readFile, writeFile)

-- ---------------------------------------------------------------------------
-- Atom walk
-- ---------------------------------------------------------------------------

-- | Walk the branch from HEAD and extract all atoms for @path@.
--   Returns Nothing if the branch doesn't exist, Just [] if file is absent.
--   Atoms are returned oldest-first.
fileAtoms
  :: SessionEffects r
  => T.Text -> FilePath -> Sem r (Maybe [FileAtom])
fileAtoms branch path =
  getBranch (BranchName branch) >>= \case
    Nothing -> return Nothing
    Just _  -> do
      mHead <- resolveRef (RefName ("refs/heads/story/" <> branch))
      case mHead of
        Nothing       -> return (Just [])
        Just headHash -> fmap Just (walkAtoms headHash [])
  where
    walkAtoms hash acc = do
      cd       <- readCommit hash
      mNew     <- blobAt (commitTree cd)
      mOld     <- case commitParents cd of
        []      -> return Nothing
        (p : _) -> readCommit p >>= blobAt . commitTree
      let acc' = maybe acc (: acc) (buildAtom hash cd mNew mOld)
      case commitParents cd of
        []      -> return acc'
        (p : _) -> walkAtoms p acc'

    blobAt treeHash = do
      mHash <- lookupPath treeHash path
      case mHash of
        Nothing -> return Nothing
        Just h  -> Just <$> readBlob h

    buildAtom hash cd mNew mOld = do
      new <- mNew
      let old     = maybe BS.empty id mOld
      if new == old then Nothing else Just FileAtom
        { atomTickId  = unObjectHash hash
        , atomContent = TE.decodeUtf8With TE.lenientDecode (BS.drop (BS.length old) new)
        , atomMessage = stripRefs (commitMessage cd)
        , atomParent  = case commitParents cd of
            (p : _) -> Just (unObjectHash p)
            []      -> Nothing
        }

    stripRefs raw = case T.lines raw of
      (l : rest) | "refs: " `T.isPrefixOf` l -> T.intercalate "\n" rest
      ls                                       -> T.intercalate "\n" ls

-- ---------------------------------------------------------------------------
-- Snapshot + dispatch
-- ---------------------------------------------------------------------------

snapshot :: ServerEnv -> T.Text -> FilePath -> IO (Either String (Maybe [FileAtom]))
snapshot env branch path = runAction env $ fileAtoms branch path

dispatch :: ServerEnv -> T.Text -> FilePath -> WS.Connection -> FileCommand -> IO ()
dispatch env branch path conn cmd = do
  let emit = WS.sendTextData conn . encode
  case cmd of
    Append _mid content -> do
      r <- runAction env (handleAppend branch path content)
      case r of
        Left err   -> emit (FileError (T.pack err))
        Right atom -> do
          notify env branch []   -- new tick appended, no id renames
          emit (AtomAppended atom)

    Read _mid -> do
      r <- runAction env (fileAtoms branch path)
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
        Right (oldId, atom) -> do
          notify env branch [(oldId, atomTickId atom)]
          emit (AtomReplaced mid oldId atom)

    DeleteAtom mid tickIdTxt -> do
      r <- runAction env (handleDeleteAtom branch path tickIdTxt)
      case r of
        Left err      -> emit (FileError (T.pack err))
        Right mapping -> do
          notify env branch mapping
          emit (AtomDeleted mid tickIdTxt mapping)

    MoveAtom mid tickIdTxt mAfterTickId -> do
      r <- runAction env (handleMoveAtom branch path tickIdTxt mAfterTickId)
      case r of
        Left err      -> emit (FileError (T.pack err))
        Right mapping -> do
          notify env branch mapping
          emit (AtomMoved mid mapping)

-- ---------------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------------

handleAppend :: SessionEffects r => T.Text -> FilePath -> T.Text -> Sem r FileAtom
handleAppend branch path content =
  withBranchSplitter @Main branch $ do
    _tids <- appendAgent @Main path content
    atomAtHead branch path

handleEditAtom
  :: SessionEffects r
  => T.Text -> FilePath -> T.Text -> T.Text
  -> Sem r (T.Text, FileAtom)
handleEditAtom branch path tickIdTxt newContent =
  withBranch @Main branch $ do
    (_newTid, _mapping) <- editAtom @Main
      (TickId tickIdTxt) path (TE.encodeUtf8 newContent)
    atom <- atomAtHead branch path
    return (tickIdTxt, atom)

handleDeleteAtom
  :: SessionEffects r
  => T.Text -> FilePath -> T.Text
  -> Sem r [(T.Text, T.Text)]
handleDeleteAtom branch _path tickIdTxt =
  withBranch @Main branch $ do
    mapping <- deleteTick @Main (TickId tickIdTxt)
    return [ (unTickId o, unTickId n) | (o, n) <- mapping ]

-- | Move an atom to a new position: delete it from its current position and
--   re-insert after the target tick using At + store.
handleMoveAtom
  :: SessionEffects r
  => T.Text -> FilePath -> T.Text -> Maybe T.Text
  -> Sem r [(T.Text, T.Text)]
handleMoveAtom _branch _path _tickIdTxt _mAfterTickId =
  fail "move not yet implemented"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Read the atom at HEAD for @path@ — used after any mutation to return the result.
atomAtHead :: SessionEffects r => T.Text -> FilePath -> Sem r FileAtom
atomAtHead branch path = do
  mHead <- resolveRef (RefName ("refs/heads/story/" <> branch))
  case mHead of
    Nothing       -> fail "branch head not found"
    Just headHash -> do
      cd   <- readCommit headHash
      mNew <- blobAt (commitTree cd)
      mOld <- case commitParents cd of
        []      -> return Nothing
        (p : _) -> readCommit p >>= blobAt . commitTree
      case mNew of
        Nothing -> fail "no blob at HEAD for this file"
        Just new -> do
          let old = maybe BS.empty id mOld
          return FileAtom
            { atomTickId  = unObjectHash headHash
            , atomContent = TE.decodeUtf8With TE.lenientDecode (BS.drop (BS.length old) new)
            , atomMessage = stripRefs (commitMessage cd)
            , atomParent  = case commitParents cd of
                (p : _) -> Just (unObjectHash p)
                []      -> Nothing
            }
  where
    blobAt treeHash = do
      mHash <- lookupPath treeHash path
      case mHash of
        Nothing -> return Nothing
        Just h  -> Just <$> readBlob h
    stripRefs raw = case T.lines raw of
      (l : rest) | "refs: " `T.isPrefixOf` l -> T.intercalate "\n" rest
      ls                                       -> T.intercalate "\n" ls
