{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | File-level business logic specific to the Writer application: running
-- the Writer/FlowWriter/Fixer prose agents. Same shape as 'Server.Core.File'
-- — plain 'Sem' functions, no JSON/WebSocket — just too specific to a
-- writing workflow to live in the generic library.
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

import Storyteller.Writer.Agent (Prompt(..), Instruction(..), ContextBlock(..))
import Storyteller.Common.Splitter (Splitter)
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
--   'Storyteller.Writer.Agent.FlowWrite').
chatWriter :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> T.Text -> [ContextItem] -> Maybe TickId -> Sem r ()
chatWriter path prompt context mFlowTid = do
  _ <- storeAs @Main (Prompt path prompt)
  let ctxBlocks = toContextBlocks context
  case mFlowTid of
    Just flowTid -> do
      info $ "flow writer agent starting: " <> T.pack path
      _ <- flowWriteAgent @(BranchTag Main) @Main path flowTid (Instruction prompt) ctxBlocks []
      info $ "flow writer agent done: " <> T.pack path
    Nothing -> do
      info $ "writer agent starting: " <> T.pack path
      _ <- writeAgent @(BranchTag Main) @Main path (Instruction prompt) ctxBlocks []
      info $ "writer agent done: " <> T.pack path

-- | Store a prompt tick then run the Fixer agent against the given targets.
chatFixer :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> T.Text -> [ContextItem] -> [TickId] -> Sem r ()
chatFixer path prompt context targets = do
  _ <- storeAs @Main (Prompt path prompt)
  info $ "fixer agent starting: " <> T.pack path
  _ <- fixAgent @(BranchTag Main) @Main path targets (Instruction prompt) (toContextBlocks context)
  info $ "fixer agent done: " <> T.pack path

toContextBlocks :: [ContextItem] -> [ContextBlock]
toContextBlocks = map (ContextBlock . ciContent)

-- | Record a character entering or leaving the scene on @path@ — presence
--   is scoped to the file (the scene), not the whole branch, see
--   'Storyteller.Writer.Types.Presence' and WRITER.md.
setPresence :: FileOpen r => FilePath -> BranchName -> PresenceEvent -> Sem r ()
setPresence path character event = void $ recordPresence @Main path character event
