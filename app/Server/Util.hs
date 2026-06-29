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
  , withBranchLLM
  ) where

import qualified Data.Text as T
import Polysemy
import Polysemy.Error (throw)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Runix.LLM (LLM)

import Server.Env (ServerEnv(..))
import Server.Run (HandlerEffects)
import Storyteller.Agent.Splitter (Splitter, splitByParagraph)
import Storyteller.Git (BranchTag(..), runBranchAndFS)
import Storyteller.Runtime (StoryModel)
import Storyteller.Storage (StoryBranch, StoryStorage, getBranch)
import Storyteller.Types (BranchName(..))

withBranch
  :: forall branch r a
  .  HandlerEffects r
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
  .  HandlerEffects r
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

-- | Open a branch with splitter and LLM in scope.
-- LLM is already interpreted by 'runRequest'; this just makes it available
-- to the inner action alongside branch FS effects.
withBranchLLM
  :: forall branch r a
  .  HandlerEffects r
  => ServerEnv -> T.Text
  -> Sem ( LLM StoryModel
         : Splitter
         : StoryBranch branch
         : FileSystemWrite (BranchTag branch)
         : FileSystemRead  (BranchTag branch)
         : FileSystem      (BranchTag branch)
         : r ) a
  -> Sem r a
withBranchLLM env b action =
  withBranch @branch env b (splitByParagraph (subsume_ action))
