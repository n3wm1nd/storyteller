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
import qualified Data.Text as T
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
--
--   An 'AssistantTurn''s trailing newlines are stripped here, not carried
--   through verbatim from storage. 'Storyteller.Common.Splitter.byParagraph'
--   is deliberately lossless — every non-final atom it produces keeps the
--   *entire* blank-line run that split it from the next paragraph, baked
--   into its own stored text (see its Haddock) — which is exactly right
--   for storage (nothing is silently rewritten) but wrong to hand back to
--   a model verbatim as its own prior turn: a model shown its own past
--   output ending in a visible @"\\n\\n"@ (or more, if it already
--   overshot) has no signal that the blank line is structural rather than
--   something worth imitating, so it starts adding its own explicit
--   blank-line padding before the next paragraph too — which gets split
--   and stored the same lossless way, then replayed back next turn,
--   compounding turn over turn. Stripped only in this model-facing
--   projection, not in storage: 'Tick.ftContent'\/the atom itself keeps
--   every byte the model actually wrote.
--
--   An 'AssistantTurn' that comes out blank after that stripping (an atom
--   whose stored content was empty or all-newlines -- an aborted or
--   genuinely empty past generation) gets 'blankAssistantPlaceholder'
--   substituted in rather than being kept as a bare @AssistantTurn ""@ or
--   dropped outright. Dropping it is exactly as wrong as keeping it empty:
--   the surrounding turns are real (a prompt was sent, another prompt
--   followed), so removing this one turn would fuse two separate 'UserTurn's
--   back to back with no role boundary between them once rendered -- the
--   same blended-into-one-user-turn failure
--   'Storyteller.Writer.Agent.Write.spliceAck' exists to prevent for the
--   splice, just reached from the other direction. A real turn boundary
--   the model actually took has to stay a turn boundary; it just can't be
--   handed back as literally nothing, which is what a blank assistant turn
--   in history reads as -- not "the assistant produced empty output that
--   time" but "produce nothing/garbage here", which is a bad thing to
--   anchor the model's own voice to right before its next continuation.
turnsFromFileTicks :: [Tick.FileTick] -> [Turn]
turnsFromFileTicks = concatMap turnOf . filter (not . ftHidden)
  where
    turnOf ft
      | Tick.ftIsType @Prompt ft = [UserTurn (Tick.ftMessage ft)]
      | Tick.ftIsType @Atom   ft =
          let content = T.dropWhileEnd (== '\n') (fromMaybe (Tick.ftMessage ft) (Tick.ftContent ft))
          in [AssistantTurn (if T.null (T.strip content) then blankAssistantPlaceholder else content)]
      | otherwise                = []

-- | Substituted for an 'AssistantTurn' whose real content came out blank
--   (see 'turnsFromFileTicks'). Deliberately not @"..."@ or silence -- a
--   short, explicit, in-voice acknowledgement that this turn produced
--   nothing, the same honesty 'Storyteller.Writer.Agent.Write.spliceAck'
--   gives a synthetic turn, just phrased for a real one instead of an
--   injected splice.
blankAssistantPlaceholder :: Text
blankAssistantPlaceholder = "(No text was written for this turn.)"

-- | 'turnsFromFileTicks' against the branch scope the caller is already
--   in, oldest turn first.
conversationTurns :: forall branch r. Member (BranchOp branch) r => FilePath -> Sem r [Turn]
conversationTurns path = turnsFromFileTicks <$> runStorage @branch (Tick.fileTicksOf path)
