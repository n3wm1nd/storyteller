{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Effect-membership vocabulary shared by any storage-backed application.
--
-- 'SessionEffects' is a declaration, not wiring — it says what a library
-- function needs, not how those effects get interpreted. The interpreters
-- that actually satisfy it (the Polysemy stack assembled around a request)
-- are app-specific assembly; see 'Server.Writer.Run.actionStack'.
module Server.Core.Run
  ( SessionEffects
  ) where

import Polysemy (Members)
import Polysemy.Error (Error)
import Polysemy.Fail (Fail)
import Runix.Git (Git)
import Runix.Logging (Logging)
import Runix.LLM (LLM)
import Runix.Random (Random)
import Runix.Time (Time, Sleep)

import Storyteller.Core.LLM.Role (ProseModel, FixerModel)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Prompt (PromptStorage)
import Storyteller.Core.Undo (Undo)

-- | Effects available at the session level (no branch open). Deliberately
--   excludes 'HTTP'/'HTTPStreaming' — handler code must only reach the
--   network through an 'LLM' effect, never directly.
--
--   Two closed, concrete 'LLM' members (not one, and not a type variable) —
--   one per role. See 'Storyteller.Core.LLM.Role': each role's model is
--   chosen independently at server startup, but that choice never surfaces
--   here or in any handler/dispatch module that merely threads 'r' through —
--   only the leaf call sites that actually invoke an agent
--   (e.g. 'Server.Writer.File') need to know which role they're using.
type SessionEffects r =
  Members '[ Random
           , Sleep
           , Time
           , Git
           , Undo
           , Fail
           , Logging
           , Error String
           , StoryStorage
           , LLM ProseModel
           , LLM FixerModel
           , PromptStorage
           ] r
