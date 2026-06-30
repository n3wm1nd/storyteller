{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Write agent: generate new prose via LLM, then commit via 'appendAgent'.
--
-- Composes: character context loading → continuationAgent → appendAgent.
module Storyteller.Agent.Write
  ( writeAgent
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, fileExists, readFile)
import Runix.Git (Git)
import Runix.LLM (LLM)
import Runix.Logging (Logging)

import Storyteller.Agent (Instruction(..), Prose(..), CharContextBlock(..), WordCount(..))
import Storyteller.Agent.Append (appendAgent)
import Storyteller.Agent.CharContext (loadCharContext)
import Storyteller.Agent.Continuation (continuationAgent)
import Storyteller.Agent.Splitter (Splitter)
import Storyteller.CLI.Env (modelConfigs)
import Storyteller.Git (BranchTag(..), runBranchAndFS)
import Storyteller.Runtime (StoryModel)
import Storyteller.Storage (StoryBranch, StoryStorage)
import Storyteller.Types (BranchName(..), TickId)

import Prelude hiding (readFile)

data CharCtx

-- | Load character context, generate prose, then append-commit each atom.
writeAgent
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[ LLM StoryModel
                , FileSystem project, FileSystemRead project, FileSystemWrite project
                , StoryBranch branch, StoryStorage, Splitter, Git, Logging, Fail ] r )
  => FilePath    -- ^ file to append to
  -> Instruction
  -> [T.Text]    -- ^ active character branch names
  -> Sem r (T.Text, [TickId])
writeAgent path instruction activeChars = do
  charContexts <- fmap concat $ mapM (\cb -> do
    blocks <- runBranchAndFS @CharCtx (BranchName cb)
            $ loadCharContext @(BranchTag CharCtx)
    return $ CharContextBlock ("## Character: " <> cb) : blocks) activeChars
  existing <- fileExists @project path >>= \case
    True  -> TE.decodeUtf8 <$> readFile @project path
    False -> return ""
  Prose generated <- continuationAgent @project @StoryModel
                       modelConfigs (Just (WordCount 300)) charContexts existing instruction
  tids <- appendAgent @branch path generated
  return (generated, tids)
