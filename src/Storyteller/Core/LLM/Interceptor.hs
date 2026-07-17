{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Turn limits as shared, transparent policy rather than a parameter
--   threaded through an agent's own recursion -- built on a fact every
--   tool-calling loop in this codebase already relies on: @history@ itself
--   is the only state that matters, growing strictly by appending, never
--   rewritten. Anything a caller would otherwise need a separate counter
--   for is already sitting right there in @history@, ready to be counted.
--
--   'withTurnBudget' intercepts 'Runix.LLM.QueryLLM' itself and, once a
--   caller-chosen condition on the growing history holds, stops actually
--   querying the model and injects a synthetic response in its place -- so
--   any loop already watching its own history for a sentinel terminates
--   the normal way, with no separate "budget ran out" branch of its own to
--   get right, and nothing to thread. Right for a hard, unconditional stop.
--
--   'toolCallsSoFar' and 'denyToolCall' are the softer counterpart, for a
--   tool-calling loop's own per-turn step: count how many tool calls
--   @history@ already carries (no separate counter -- it's a pure read of
--   what's already there), and once that count reaches a limit, hand back
--   a denial instead of really executing the next one -- a real, visible
--   reply to that attempt, not a swapped-out response the model never
--   asked for.
module Storyteller.Core.LLM.Interceptor
  ( withInterception
  , withTurnBudget
  , withToolCallBudget
  , toolCallsSoFar
  , denyToolCall
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Polysemy
import Polysemy.Fail (Fail)

import Runix.LLM (LLM(..), Message(..))
import UniversalLLM (HasTools, ToolCall, ToolResult(..))

-- | The general mechanism underneath 'withTurnBudget': intercept every
--   'Runix.LLM.QueryLLM' call, and once @shouldIntervene history@ is true
--   for the growing history so far, stop actually querying the model and
--   run @respond history@ instead to produce what comes back in its place.
--   Turn count past a budget is only one condition ('withTurnBudget');
--   "does the history already contain N tool calls," "has this specific
--   message appeared," or anything else over the same @[Message model]@ is
--   just a different @shouldIntervene@, and @respond@ deciding whether to
--   hand back a synthetic message, look something up, or 'Polysemy.Fail.fail'
--   outright is what let 'withTurnBudget' build its own "don't inject the
--   sentinel twice" safety check purely as an instance of this, not a
--   special case baked in here.
--
--   Cross-cutting behaviour a caller's own loop doesn't have to know exists
--   -- the same way @Agent.Integration.Harness.assertToolCallBudget@ taps
--   the same effect for the agent-integration test suite's own
--   cross-cutting concerns (recording, fail-fast retries).
withInterception
  :: forall model r a
  .  Member (LLM model) r
  => ([Message model] -> Bool)                     -- ^ condition on the history so far
  -> ([Message model] -> Sem r [Message model])    -- ^ what to return instead, given that history
  -> Sem r a -> Sem r a
withInterception shouldIntervene respond = intercept @(LLM model) recorder
  where
    recorder :: forall m x. LLM model m x -> Sem r x
    recorder (QueryLLM configs history)
      | shouldIntervene history = Right <$> respond history
      | otherwise               = send (QueryLLM configs history)

-- | Cap a multi-turn conversation at @maxTurns@ actual model queries: once
--   reached, inject a synthetic response containing @sentinel@ instead of
--   querying the model again, so a loop already watching for that sentinel
--   terminates the normal way, with no separate "budget ran out" branch of
--   its own to get right.
--
--   Refuses (via 'Polysemy.Fail.Fail') to inject the sentinel a second time
--   -- if the caller's own loop isn't stopping once the sentinel shows up,
--   injecting it again would just spin forever instead of actually
--   terminating anything.
withTurnBudget
  :: forall model r a
  .  Members '[LLM model, Fail] r
  => Int   -- ^ turn budget: queries beyond this get the sentinel instead of a real response
  -> Text  -- ^ the sentinel to inject once the budget is reached
  -> Sem r a -> Sem r a
withTurnBudget maxTurns sentinel = withInterception @model overBudget respond
  where
    overBudget = (>= maxTurns) . length . filter isAssistantTurn

    respond history
      | sentinel `T.isInfixOf` mconcat [ t | AssistantText t <- history ] =
          fail "withTurnBudget: sentinel already in history but the loop kept going -- refusing to inject it again"
      | otherwise = pure [AssistantText sentinel]

    isAssistantTurn (AssistantText _)      = True
    isAssistantTurn (AssistantTool _)      = True
    isAssistantTurn (AssistantReasoning _) = True
    isAssistantTurn (AssistantJSON _)      = True
    isAssistantTurn _                      = False

-- | Cap how many tool calls a conversation gets to make, entirely inside
--   one 'Runix.LLM.QueryLLM' interception -- a caller's own loop calls
--   'Runix.LLM.queryLLM' exactly the way it always has (no budget check, no
--   counter, nothing) and only ever sees a clean, final response for that
--   turn. What happens underneath once @softLimit@ tool calls have already
--   been made: the real model is still queried as normal ("pass it on"),
--   but if *that* response tries to call a tool again, this doesn't hand
--   the attempt back to the caller at all -- it appends a 'denyToolCall'
--   error for each attempted call and queries the model again itself,
--   right here, transparently. The caller never sees that extra round
--   trip; it only ever gets back a response with no tool calls left in it
--   to execute, or a fresh one under budget it can execute for real.
--
--   Bounded by @hardRounds@ -- consecutive denial rounds within one
--   'Runix.LLM.QueryLLM' call, not the conversation's turn count -- so a
--   model that keeps trying anyway can't spin this forever; past that it
--   gives up via 'Polysemy.Fail.Fail' rather than looping indefinitely.
withToolCallBudget
  :: forall model r a
  .  (HasTools model, Members '[LLM model, Fail] r)
  => Int   -- ^ soft limit: tool calls beyond this get denied instead of reaching the caller
  -> Int   -- ^ hard limit: consecutive denial rounds tolerated before giving up
  -> Sem r a -> Sem r a
withToolCallBudget softLimit hardRounds = intercept @(LLM model) recorder
  where
    recorder :: forall m x. LLM model m x -> Sem r x
    recorder (QueryLLM configs history) = go (0 :: Int) history
      where
        go rounds h = do
          result <- send (QueryLLM configs h)
          case result of
            Left err -> pure (Left err)
            Right response ->
              let calls = [ tc | AssistantTool tc <- response ]
              in if null calls || toolCallsSoFar h < softLimit
                   then pure (Right response)
                   else if rounds >= hardRounds
                     then fail "withToolCallBudget: still attempting tool calls after the hard denial-round limit, giving up"
                     else go (rounds + 1) (h ++ response ++ [ ToolResultMsg (denyToolCall tc) | tc <- calls ])

-- | How many tool calls @history@ already carries -- every one the model
--   has attempted so far, successful or not, counted straight off the
--   conversation itself rather than a counter a loop would otherwise have
--   to maintain and pass down through its own recursion.
toolCallsSoFar :: [Message model] -> Int
toolCallsSoFar history = length [ () | AssistantTool _ <- history ]

-- | The denial a tool-calling loop hands back, in place of really running
--   the call, once 'toolCallsSoFar' says it's time -- pure, so the loop's
--   own per-turn step still decides and executes that choice directly
--   (@if toolCallsSoFar history >= softLimit then pure (denyToolCall tc)
--   else executeToolCallFromList tools tc@); only the wording is shared, so
--   every agent's denial reads the same way to a model that hits it.
denyToolCall :: ToolCall -> ToolResult
denyToolCall tc = ToolResult tc (Left denialMessage)
  where
    denialMessage = "Turn budget reached: no more tool calls are available for this task. \
                     \Answer directly now, in plain text, using what you already have."
