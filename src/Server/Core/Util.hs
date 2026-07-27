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

import qualified Storyteller.Core.Branch as Branch
import Storyteller.Core.Branch (Branches)
import Storyteller.Core.Git (BranchTag(..), BranchOp, runStoryFSGit)
import Storyteller.Core.Storage (StoryStorage, getBranch)
import Storyteller.Core.Types (BranchName(..))
import Polysemy.Fail (Fail)

-- | Open a branch's storage/filesystem scope. Callers that open this once
--   for a whole connection's lifetime (see 'Server.Writer.File.Connection',
--   'Server.Writer.Branch.Connection') and dispatch many commands through it
--   should wrap each individual command in 'Storyteller.Core.Git.withStorage'
--   themselves — wrapping the whole long-lived scope here would buffer
--   every command's ref writes together, only publishing (and therefore
--   notifying) once the connection closes.
--   Asks for 'Storyteller.Core.Branch.Branches' -- the capability to enter
--   a branch -- rather than 'Runix.Git.Git'. It used to call
--   'Storyteller.Core.Git.runBranchAndFS' directly, and calling an
--   interpreter inline means inheriting that interpreter's own
--   dependencies: every caller of this, which is nearly every connection
--   handler, had to declare @Git@ to open a branch. They were not doing
--   git; they were entering a branch and then reading and writing files.
--   Now they say exactly that, and which backend answers is
--   'Storyteller.Core.Git.runBranchesGit''s business, wired once.
withBranch
  :: forall branch r a
  .  Members '[StoryStorage, Error String, Branches, Fail] r
  => T.Text
  -> Sem ( FileSystemWrite (BranchTag branch)
         : FileSystemRead  (BranchTag branch)
         : FileSystem      (BranchTag branch)
         : BranchOp branch
         : r ) a
  -> Sem r a
withBranch b action = do
  let name = BranchName b
  getBranch name >>= \case
    Nothing -> throw ("branch not found: " <> T.unpack b)
    Just _  -> Branch.withBranch @branch name (runStoryFSGit @branch name action)
