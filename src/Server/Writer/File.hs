{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | File-level business logic specific to the Writer application: this is
-- the seam between routing ('Server.Writer.File.Dispatch') and the
-- Writer/FlowWriter/Fixer prose agents, which are themselves effect-minimal
-- (LLM-only where possible). Gathering file/character context, appending
-- generated prose, and choosing Writer vs. Fixer when there are no targets
-- all happen here rather than inside the agents — same shape as
-- 'Server.Core.File': plain 'Sem' functions, no JSON/WebSocket.
module Server.Writer.File
  ( chatWriter
  , chatFixer
  , setPresence
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import Polysemy (Member, Sem)
import Runix.Logging (info)

import Server.Core.File (FileOpen)
import Server.Core.Run (SessionEffects)
import Server.Writer.File.Protocol (ContextItem(..))

import Storyteller.Writer.Agent (Prompt(..), Instruction(..), ContextBlock(..), Prose(..))
import Storyteller.Common.Splitter (Splitter, splitAtoms)
import Storyteller.Core.Append (append)
import Storyteller.Writer.Agent.Continuation (gatherFileContext)
import Storyteller.Writer.Agent.Write (writeAgent)
import Storyteller.Writer.Agent.FlowWrite (flowWriteAgent)
import Storyteller.Writer.Agent.Fix (fixAgent)
import Storyteller.Writer.Presence (recordPresence)
import Storyteller.Writer.Types (PresenceEvent)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (storeAs)
import Storyteller.Core.Types (BranchName, TickId(..))
import Storyteller.Core.Git (BranchTag)

-- | Store a prompt tick then run Writer, or FlowWriter when 'mFlowTid' is
--   set (the tick that was HEAD when the user started typing — see
--   'Storyteller.Writer.Agent.FlowWrite'). Context (existing file content,
--   branch files, character summaries) is gathered here and handed to the
--   agents as plain data; they append nothing themselves, so appending the
--   result is done here too. No character branches are wired into a scene
--   yet, hence the empty list below — see WRITER.md presence conventions
--   for where that's headed.
chatWriter :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> T.Text -> [ContextItem] -> Maybe TickId -> Sem r ()
chatWriter path prompt context mFlowTid = do
  _ <- storeAs @Main (Prompt path prompt)
  (existing, fileCtx) <- gatherFileContext @(BranchTag Main) path
  let extraContext = toContextBlocks context <> fileCtx
      instruction  = Instruction prompt
  case mFlowTid of
    Just flowTid -> do
      info $ "flow writer agent starting: " <> T.pack path
      (_reworked, Prose generated) <- flowWriteAgent @(BranchTag Main) @Main path flowTid existing extraContext instruction []
      _ <- mapM (append @Main path) =<< splitAtoms generated
      info $ "flow writer agent done: " <> T.pack path
    Nothing -> do
      info $ "writer agent starting: " <> T.pack path
      Prose generated <- writeAgent existing extraContext instruction []
      _ <- mapM (append @Main path) =<< splitAtoms generated
      info $ "writer agent done: " <> T.pack path

-- | Store a prompt tick then run the Fixer agent against the given targets.
--   With no targets, there's nothing to rework — that's a different policy
--   ("just write") from the Fixer's, so it's handled here as a fall through
--   to the same Writer path 'chatWriter' takes, rather than living inside
--   'Storyteller.Writer.Agent.Fix.fixAgent'.
chatFixer :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> T.Text -> [ContextItem] -> [TickId] -> Sem r ()
chatFixer path prompt context [] = chatWriter path prompt context Nothing
chatFixer path prompt _context targets = do
  _ <- storeAs @Main (Prompt path prompt)
  info $ "fixer agent starting: " <> T.pack path
  _ <- fixAgent @(BranchTag Main) @Main path targets (Instruction prompt)
  info $ "fixer agent done: " <> T.pack path

toContextBlocks :: [ContextItem] -> [ContextBlock]
toContextBlocks = map (ContextBlock . ciContent)

-- | Record a character entering or leaving the scene on @path@ — presence
--   is scoped to the file (the scene), not the whole branch, see
--   'Storyteller.Writer.Types.Presence' and WRITER.md.
setPresence :: FileOpen r => FilePath -> BranchName -> PresenceEvent -> Sem r ()
setPresence path character event = void $ recordPresence @Main path character event
