{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Character/entity tracker agent.
--
-- Copies atoms from a trackee branch into a tracker branch, maintaining
-- cross-branch references.
--
-- Each function takes two pairs of type parameters:
--   @trackeeProject@  — FS phantom for the source  (e.g. @BranchTag Source@)
--   @trackerProject@  — FS phantom for the dest     (e.g. @BranchTag Tracker@)
--   @trackerBranch@   — StoryBranch phantom for dest (e.g. @Tracker@)
-- where @trackerProject ~ BranchTag trackerBranch@.
module Storyteller.Agent.Tracker
  ( trackBranch
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T

import Polysemy
import Polysemy.Fail
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, fileExists, readFile, writeFile)
import Storyteller.Git (BranchTag(..))
import Storyteller.Storage (StoryBranch, StoryStorage, get, store)
import Storyteller.Types (Tick(..), TickId(..))

import Prelude hiding (readFile, writeFile)

-- | Copy any atoms from trackee files not yet reflected in the tracker.
--
-- Type application: @trackBranch \@trackeeProject \@trackerProject \@trackerBranch@
-- where @trackerProject ~ BranchTag trackerBranch@.
trackBranch
  :: forall trackeeProject trackerProject trackerBranch r
  .  ( trackerProject ~ BranchTag trackerBranch
     , Members '[ FileSystem     trackeeProject
                , FileSystemRead trackeeProject
                , FileSystem     trackerProject
                , FileSystemRead trackerProject
                , FileSystemWrite trackerProject
                , StoryBranch trackerBranch
                , StoryStorage
                , Fail
                ] r )
  => Tick        -- ^ current head tick of the trackee (for cross-reference messages)
  -> [FilePath]
  -> Sem r [TickId]
trackBranch trackeeTick files =
  fmap concat $ mapM (trackFile @trackeeProject @trackerProject @trackerBranch trackeeTick) files

trackFile
  :: forall trackeeProject trackerProject trackerBranch r
  .  ( trackerProject ~ BranchTag trackerBranch
     , Members '[ FileSystem     trackeeProject
                , FileSystemRead trackeeProject
                , FileSystem     trackerProject
                , FileSystemRead trackerProject
                , FileSystemWrite trackerProject
                , StoryBranch trackerBranch
                , StoryStorage
                , Fail
                ] r )
  => Tick
  -> FilePath
  -> Sem r [TickId]
trackFile trackeeTick path = do
  trackeeContent <- readFromBranch @trackeeProject path
  trackerContent <- readFromBranch @trackerProject path

  let alreadyCopied = BS.length trackerContent
      newSuffix     = BS.drop alreadyCopied trackeeContent

  if BS.null newSuffix
    then return []
    else do
      writeFile @trackerProject path (trackerContent <> newSuffix)
      let msg = "track: " <> T.pack path <> " from " <> unTickId (tickId trackeeTick)
      tid <- store @trackerBranch msg
      return [tid]

readFromBranch
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => FilePath
  -> Sem r ByteString
readFromBranch path = do
  exists <- fileExists @project path
  if exists then readFile @project path else return BS.empty
