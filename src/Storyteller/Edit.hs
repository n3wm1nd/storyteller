{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Chain editing operations: delete, edit, and move atoms in a branch.
--
-- These compose the storage primitives (At, Drop, WithFS, Reset) into
-- coherent chain mutations. They belong below the agent layer — no LLM
-- or splitter involvement — but above raw storage, since they understand
-- file content.
module Storyteller.Edit
  ( storeAtom
  , deleteTick
  , editAtom
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Polysemy
import Polysemy.Fail

import Runix.FileSystem ( FileSystem, FileSystemRead, FileSystemWrite, appendFile )
import Storyteller.Git (BranchTag)
import Storyteller.Storage (StoryBranch, StoryStorage, at, withFS, drop, reset, store)
import Storyteller.Types (TickId)

import Prelude hiding (appendFile, drop)

-- | Write @content@ to @path@ and commit it as an atom tick.
--   The commit message is the content truncated to 60 chars.
storeAtom
  :: forall branch r
  .  ( Members '[ StoryBranch branch
                , FileSystem      (BranchTag branch)
                , FileSystemRead  (BranchTag branch)
                , FileSystemWrite (BranchTag branch)
                , StoryStorage
                , Fail ] r )
  => FilePath -> BS.ByteString -> Sem r TickId
storeAtom path newBytes = do
  appendFile @(BranchTag branch) path newBytes
  store @branch (T.take 60 (TE.decodeUtf8With TE.lenientDecode newBytes))

-- | Remove a tick from the chain. At rewinds to it, Drop removes it,
--   At replays the tail onto the parent. Reset syncs the working tree.
--   Returns the old→new id mapping for all tail ticks.
deleteTick
  :: forall branch r
  .  Members '[StoryBranch branch, StoryStorage, Fail] r
  => TickId
  -> Sem r [(TickId, TickId)]
deleteTick tid = do
  (_unit, mapping) <- at @branch tid $ drop @branch
  reset @branch
  return mapping

-- | Replace an atom's content in-place. Rewinds to the tick, drops it,
--   enters the FS at the parent state, appends the new content and stores.
--   At replays the tail onto the new atom. Reset syncs the working tree.
--   Returns (oldTickId, newTickId) plus the tail mapping.
editAtom
  :: forall branch r
  .  ( Members '[ StoryBranch branch
                , FileSystem      (BranchTag branch)
                , FileSystemRead  (BranchTag branch)
                , FileSystemWrite (BranchTag branch)
                , StoryStorage
                , Fail ] r )
  => TickId
  -> FilePath
  -> BS.ByteString   -- ^ new atom content (raw bytes, suffix only)
  -> Sem r (TickId, [(TickId, TickId)])
editAtom tid path newBytes = do
  (newTid, mapping) <- at @branch tid $ do
    drop @branch
    withFS @branch $ storeAtom @branch path newBytes
  reset @branch
  return (newTid, mapping)
