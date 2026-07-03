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
  ) where

import qualified Data.Text as T
import Polysemy (Member, Sem)
import Runix.Logging (info)

import Server.Core.File (FileOpen)
import Server.Core.Run (SessionEffects)
import Server.Writer.File.Protocol (ContextItem(..))

import Storyteller.Agent (Prompt(..), Instruction(..), ContextBlock(..))
import Storyteller.Agent.Splitter (Splitter)
import Storyteller.Agent.Write (writeAgent)
import Storyteller.Agent.FlowWrite (flowWriteAgent)
import Storyteller.Agent.Fix (fixAgent)
import Storyteller.Runtime (Main)
import Storyteller.Storage (storeAs)
import Storyteller.Types (TickId(..))
import Storyteller.Git (BranchTag)

-- | Store a prompt tick then run Writer, or FlowWriter when 'mFlowTid' is
--   set (the tick that was HEAD when the user started typing — see
--   'Storyteller.Agent.FlowWrite').
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
