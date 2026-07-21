{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Shared harness for every roleplay-writer scenario in this suite: a
--   local replica of 'Server.Writer.File.roleplayWriter' -- see
--   'Agent.Integration.Journey'\'s own module Haddock for why this suite
--   never calls through 'Server.Writer.File' directly (that module is
--   pinned to production's role routing, which would defeat the point of
--   swapping in @STORY_MODEL@\/@JUDGE_MODEL@ per run).
module Agent.Integration.RoleplayHarness
  ( runRoleplayTurn
  ) where

import Control.Monad (void)
import qualified Data.Text as T

import Polysemy (Members, Sem)
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead)
import Runix.Git (Git)
import Runix.Logging (Logging)

import qualified Storage.Ops as Ops
import Storyteller.Common.Splitter (Splitter, splitAtoms)
import Storyteller.Context.DSL.Rendering (RenderedContext(..))
import Storyteller.Core.Git (BranchOp, BranchTag, runBranchAndFS, runStorage)
import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Core.Prompt (PromptStorage)
import Storyteller.Core.Context (ContextStorage)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent (CharLabel(..), Prose(..))
import Storyteller.Writer.Agent.CharContext (charSummaryFull)
import Storyteller.Writer.Agent.Context (WorldContext(..))
import Storyteller.Writer.Agent.Roleplay (roleplayAgent, characterReflectAgent)
import Storyteller.Writer.Branches (branchDisplayName)
import Storyteller.Writer.Presence (activeCharactersFor)
import Storyteller.Writer.Types (Character(..))

-- | Phantom tag for opening one active character branch's filesystem at a
--   time, dynamically -- local to this module, same role
--   'Server.Writer.File.ActiveChar' plays in production.
data ActiveChar

-- | Run one roleplay turn against @path@: interrogate every character
--   'Storyteller.Writer.Presence.activeCharactersFor' finds present via
--   'Storyteller.Writer.Agent.Roleplay.roleplayAgent', append the finished
--   scene, then run 'Storyteller.Writer.Agent.Roleplay.characterReflectAgent'
--   once per active character. Returns every present character's own new
--   journal entry alongside the narrative, so a caller can inspect each one
--   directly rather than re-reading them back off the chain.
runRoleplayTurn
  :: forall r
  .  ( LLMs r
     , Members '[ PromptStorage, ContextStorage, Git, StoryStorage, BranchOp Main, Splitter
                , FileSystem (BranchTag Main), FileSystemRead (BranchTag Main)
                , Logging, Fail] r
     )
  => FilePath -> T.Text -> Sem r (Prose, [(CharLabel, T.Text)])
runRoleplayTurn path prompt = do
  active <- activeCharactersFor @Main path
  let characters = [ (CharLabel (characterLabel c), c) | c <- active ]
  Prose text <- roleplayAgent (WorldContext (Node [] [])) characters prompt
  sceneRefs <- mapM (\c -> runStorage @Main (Ops.append path c)) =<< splitAtoms text
  entries <- case sceneRefs of
    [] -> pure []
    _  -> mapM (reflectFor text (last sceneRefs)) active
  pure (Prose text, entries)
  where
    characterLabel (Character (BranchName name)) = branchDisplayName name

    reflectFor narrative sceneRef character@(Character branch) = do
      entry <- runBranchAndFS @ActiveChar branch $ do
        ownContext <- charSummaryFull @(BranchTag ActiveChar) (const True)
        entry <- characterReflectAgent @(BranchTag ActiveChar) (characterLabel character) ownContext narrative
        void $ runStorage @ActiveChar (Ops.addAtomWithRefs [sceneRef] "journal.md" entry)
        pure entry
      pure (CharLabel (characterLabel character), entry)
