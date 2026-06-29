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

import Server.Env (ServerEnv)
import Server.File.Protocol
import Server.Run (runAction, SessionEffects)
import Server.Util (withBranchSplitter)

import Storyteller.Agent.Append (appendAgent)
import Storyteller.Git (BranchTag)
import Storyteller.Storage (getBranch)
import Storyteller.Types (BranchName(..))

import Runix.Git
  ( ObjectHash(..), CommitData(..)
  , readBlob, resolveRef, readCommit, lookupPath
  , RefName(..)
  )

import Prelude

data Main

-- ---------------------------------------------------------------------------
-- Atom walk
-- ---------------------------------------------------------------------------

-- | Walk the branch from HEAD and extract all atoms for @path@.
--   Returns Nothing if the branch doesn't exist, Just [] if file is absent.
--   Atoms are returned oldest-first; content is the suffix beyond parent's blob.
fileAtoms
  :: SessionEffects r
  => T.Text    -- ^ branch name
  -> FilePath
  -> Sem r (Maybe [FileAtom])
fileAtoms branch path = do
  getBranch (BranchName branch) >>= \case
    Nothing -> return Nothing
    Just _  -> do
      mHead <- resolveRef (RefName ("refs/heads/story/" <> branch))
      case mHead of
        Nothing       -> return (Just [])
        Just headHash -> do
          atoms <- walkAtoms headHash []
          return $ Just atoms
  where
    -- Walk backwards from @hash@, prepend atoms (yielding newest-first); reverse at top.
    walkAtoms hash acc = do
      cd <- readCommit hash
      mNewBlob <- blobAt (commitTree cd)
      mOldBlob <- case commitParents cd of
        []      -> return Nothing
        (p : _) -> readCommit p >>= blobAt . commitTree
      let atom = buildAtom hash cd mNewBlob mOldBlob
          acc' = maybe acc (: acc) atom
      case commitParents cd of
        []      -> return acc'
        (p : _) -> walkAtoms p acc'

    blobAt treeHash = do
      mHash <- lookupPath treeHash path
      case mHash of
        Nothing -> return Nothing
        Just h  -> Just <$> readBlob h

    buildAtom hash cd mNew mOld =
      case mNew of
        Nothing  -> Nothing
        Just new ->
          let old     = maybe BS.empty id mOld
              changed = new /= old
          in if not changed then Nothing else Just FileAtom
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

-- | Initial snapshot sent on connect.
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
        Right atom -> emit (AtomAppended atom)

    Read _mid -> do
      r <- runAction env (fileAtoms branch path)
      case r of
        Left err           -> emit (FileError (T.pack err))
        Right Nothing      -> emit (FileAbsent Nothing)
        Right (Just atoms) -> emit (FileAtoms atoms)

    Delete _mid ->
      emit (FileError "delete not yet implemented")

-- ---------------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------------

handleAppend :: SessionEffects r => T.Text -> FilePath -> T.Text -> Sem r FileAtom
handleAppend branch path _content =
  withBranchSplitter @Main branch $ do
    _tickId <- appendAgent @(BranchTag Main) @Main path _content
    mHead   <- resolveRef (RefName ("refs/heads/story/" <> branch))
    case mHead of
      Nothing       -> fail "branch head disappeared after append"
      Just headHash -> do
        cd       <- readCommit headHash
        mNewBlob <- blobAt (commitTree cd)
        mOldBlob <- case commitParents cd of
          []      -> return Nothing
          (p : _) -> readCommit p >>= blobAt . commitTree
        case mNewBlob of
          Nothing -> fail "appended file has no blob"
          Just new -> do
            let old = maybe BS.empty id mOldBlob
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
