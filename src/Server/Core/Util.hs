{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Core.Util
  ( withBranch
  ) where

import qualified Data.Text as T
import Polysemy
import Polysemy.Error (Error, throw)
import Runix.Git (Git)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)

import Storyteller.Git (BranchTag(..), runBranchAndFS)
import Storyteller.Storage (StoryBranch, StoryStorage, getBranch)
import Storyteller.Types (BranchName(..))
import Polysemy.Fail (Fail)

-- | Open a branch's storage/filesystem scope. Callers that open this once
--   for a whole connection's lifetime (see 'Server.Writer.File.Connection',
--   'Server.Writer.Branch.Connection') and dispatch many commands through it
--   should wrap each individual command in 'Storyteller.Git.withStorage'
--   themselves — wrapping the whole long-lived scope here would buffer
--   every command's ref writes together, only publishing (and therefore
--   notifying) once the connection closes.
withBranch
  :: forall branch r a
  .  Members '[StoryStorage, Error String, Git, Fail] r
  => T.Text
  -> Sem ( StoryBranch branch
         : FileSystemWrite (BranchTag branch)
         : FileSystemRead  (BranchTag branch)
         : FileSystem      (BranchTag branch)
         : r ) a
  -> Sem r a
withBranch b action = do
  let name = BranchName b
  getBranch name >>= \case
    Nothing -> throw ("branch not found: " <> T.unpack b)
    Just _  -> runBranchAndFS @branch name action
