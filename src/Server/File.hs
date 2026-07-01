{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | File-level business logic.
--
-- Pure (or limited-effect) functions that implement file operations.
-- No JSON, no WebSocket, no T.Text ids — callers handle the boundary.
-- These functions are the unit under test.
module Server.File
  ( fileState
  , appendToFile
  , editFileAtom
  , deleteFileAtom
  , moveFileAtom
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy (Members, Sem)
import Polysemy.Error (Error)
import Polysemy.Fail (Fail)
import Runix.Git (Git)
import Runix.Logging (Logging, info)

import Server.Protocol (Update(..), toWireTick)
import Server.Run (SessionEffects)
import Server.Util (withBranch, withBranchSplitter)

import Storyteller.Agent.Append (appendAgent)
import Storyteller.Runtime (Main)
import qualified Storyteller.Storage as Storage
import Storyteller.Storage (FileTick, StoryStorage, fileTicks, getBranch)
import Storyteller.Edit (deleteTick, editAtom, moveTick)
import Storyteller.Types (BranchName(..), TickId(..))

-- ---------------------------------------------------------------------------
-- State query
-- ---------------------------------------------------------------------------

-- | Full file state: all ticks for this path and current HEAD id.
--   Returns Nothing if the branch doesn't exist, Just (Nothing, _) impossible —
--   absent file returns Just with empty Update (head = "").
fileState
  :: Members '[StoryStorage, Git, Error String, Fail] r
  => BranchName -> FilePath -> Sem r (Maybe Update)
fileState (BranchName n) path =
  getBranch (BranchName n) >>= \case
    Nothing -> return Nothing
    Just _  -> withBranch @Main n $
      fmap (Just . fileUpdate) (fileTicks @Main path)

-- ---------------------------------------------------------------------------
-- Mutations
-- ---------------------------------------------------------------------------

-- | Append content to a file, splitting into atoms via the append agent.
appendToFile :: SessionEffects r => BranchName -> FilePath -> T.Text -> Sem r ()
appendToFile (BranchName n) path content =
  withBranchSplitter @Main n $ do
    info $ "appending to: " <> T.pack path
    void $ appendAgent @Main path content
    info $ "append done: " <> T.pack path

-- | Replace an atom's content in-place.
editFileAtom
  :: Members '[StoryStorage, Git, Error String, Fail] r
  => BranchName -> FilePath -> TickId -> T.Text -> Sem r ()
editFileAtom (BranchName n) path tid content =
  withBranch @Main n $
    void $ editAtom @Main tid path (TE.encodeUtf8 content)

-- | Delete an atom from the file's chain.
deleteFileAtom
  :: Members '[StoryStorage, Git, Error String, Fail] r
  => BranchName -> FilePath -> TickId -> Sem r ()
deleteFileAtom (BranchName n) _path tid =
  withBranch @Main n $
    void $ deleteTick @Main tid

-- | Move an atom to a new position in the file's chain.
moveFileAtom
  :: Members '[StoryStorage, Git, Error String, Fail] r
  => BranchName -> FilePath -> TickId -> Maybe TickId -> Sem r ()
moveFileAtom (BranchName n) _path tid mAfter =
  withBranch @Main n $
    void $ moveTick @Main tid mAfter

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

fileUpdate :: [FileTick] -> Update
fileUpdate ticks = Update
  { updateTicks = map toWireTick ticks
  , updateHead  = case reverse ticks of
                    []    -> ""
                    (t:_) -> Storage.ftTickId t
  }
