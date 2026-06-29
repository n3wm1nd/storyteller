{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Util
  ( withBranch
  , withBranchSplitter
  ) where

import qualified Data.Text as T
import Polysemy
import Polysemy.Error (throw)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)

import Server.Env (ServerEnv(..))
import Server.Run (SessionEffects)
import Storyteller.Agent.Splitter (Splitter, splitByParagraph)
import Storyteller.Git (BranchTag(..), runBranchAndFS)
import Storyteller.Storage (StoryBranch, getBranch)
import Storyteller.Types (BranchName(..))

withBranch
  :: forall branch r a
  .  SessionEffects r
  => ServerEnv -> T.Text
  -> Sem ( StoryBranch branch
         : FileSystemWrite (BranchTag branch)
         : FileSystemRead  (BranchTag branch)
         : FileSystem      (BranchTag branch)
         : r ) a
  -> Sem r a
withBranch _env b action = do
  let name = BranchName b
  getBranch name >>= \case
    Nothing -> throw ("branch not found: " <> T.unpack b)
    Just _  -> runBranchAndFS @branch name action

withBranchSplitter
  :: forall branch r a
  .  SessionEffects r
  => ServerEnv -> T.Text
  -> Sem ( Splitter
         : StoryBranch branch
         : FileSystemWrite (BranchTag branch)
         : FileSystemRead  (BranchTag branch)
         : FileSystem      (BranchTag branch)
         : r ) a
  -> Sem r a
withBranchSplitter env b action =
  withBranch @branch env b (splitByParagraph action)
