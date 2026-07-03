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

import Storyteller.Runtime (StoryModel)
import Storyteller.Storage (StoryStorage)

-- | Effects available at the session level (no branch open). Deliberately
--   excludes 'HTTP'/'HTTPStreaming' — handler code must only reach the
--   network through the 'LLM' effect, never directly.
type SessionEffects r =
  Members '[Random, Sleep, Time, Git, Fail, Logging, Error String, StoryStorage, LLM StoryModel] r
