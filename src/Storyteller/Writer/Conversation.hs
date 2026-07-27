{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A file's own tick history read as a conversation.
--
-- A chat file is not stored as a transcript — it is stored as ticks, the
-- same as everything else — so \"what were the turns\" is a derivation, and
-- this module owns it. It owns it in exactly one place on purpose: the
-- @\"prompt\"@\/@\"atom\"@ mapping and the hidden-tick rule were, until
-- this module existed, written out twice (once for the LLM-facing
-- 'Storyteller.Writer.Agent.Chat.historyFromFileTicks', once for the
-- context DSL's own @readconversation@) in two different message types,
-- with nothing tying the two copies together.
--
-- 'Turn' is that tie: a model-agnostic turn, neither a
-- @UniversalLLM.Message@ nor a 'Storyteller.Context.DSL.Value.Message'.
-- Both of those are one projection away ('Chat.historyFromFileTicks',
-- @Compile.turnToMessage@), and neither gets to be the definition.
--
-- The hidden flag ('Storyteller.Common.Types.ftHidden', toggled per atom
-- from the UI via 'Server.Core.File.hideFileAtoms') is honoured here, which
-- makes this the one place a hidden tick actually stops reaching a model.
module Storyteller.Writer.Conversation
  ( Turn(..)
  , turnsFromFileTicks
  , conversationTurns
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Polysemy

import qualified Storage.Tick as Tick
import Storyteller.Common.Types (ftHidden)
import Storyteller.Core.Atom (Atom)
import Storyteller.Core.Branch (BranchOp, runStorage)
import Storyteller.Writer.Agent (Prompt)

-- | One turn of a file's tick-history-as-conversation. Deliberately not
--   the raw 'Storage.Tick.FileTick': @ftKind@\/@ftFields@ are storage
--   vocabulary, and a caller asking \"what was said\" should not have to
--   know them.
data Turn = UserTurn Text | AssistantTurn Text
  deriving (Eq, Show)

-- | The whole derivation, pure: prompts are the user's turns, atoms the
--   assistant's (preferring 'Tick.ftContent', the full text, over the
--   possibly-elided 'Tick.ftMessage'), every other kind on the file --
--   notes, presence -- is conversational noise and dropped, as is anything
--   explicitly hidden.
turnsFromFileTicks :: [Tick.FileTick] -> [Turn]
turnsFromFileTicks = concatMap turnOf . filter (not . ftHidden)
  where
    turnOf ft
      | Tick.ftIsType @Prompt ft = [UserTurn (Tick.ftMessage ft)]
      | Tick.ftIsType @Atom   ft = [AssistantTurn (fromMaybe (Tick.ftMessage ft) (Tick.ftContent ft))]
      | otherwise                = []

-- | 'turnsFromFileTicks' against the branch scope the caller is already
--   in, oldest turn first.
conversationTurns :: forall branch r. Member (BranchOp branch) r => FilePath -> Sem r [Turn]
conversationTurns path = turnsFromFileTicks <$> runStorage @branch (Tick.fileTicksOf path)
