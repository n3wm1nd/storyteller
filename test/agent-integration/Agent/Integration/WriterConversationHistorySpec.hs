{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Does 'writeAgent''s @currentTicks@ argument -- this chapter's own
--   conversation so far, reconstructed via
--   'Storyteller.Writer.Agent.Chat.historyFromFileTicks' into alternating
--   'UniversalLLM.UserText'\/'UniversalLLM.AssistantText' turns (see its own
--   Haddock) -- actually carry a fact from an earlier turn *in the same
--   file* into a later one? Distinct from
--   'Agent.Integration.WriterEarlierChaptersSpec' (a fact from a different
--   file/chapter) and from the journal specs (a fact from a different
--   branch): this is the one channel that's this file's own prior
--   Prompt\/Atom tick pairs, the same shape 'Server.Writer.File.chatWriter'
--   itself replays every call.
--
--   Two real 'writeAgent' calls against the same file, replicating
--   'Server.Writer.File.chatWriter''s own store-prompt-then-gather-then-call
--   sequence directly (see 'Agent.Integration.Journey.writeChat' for the
--   same pattern) rather than going through the handler itself (same
--   reasoning as 'Agent.Integration.Journey''s own module Haddock: that
--   would pin production's own model routing). Turn one establishes a
--   detail in passing; turn two's instruction never repeats it. Two real
--   LLM calls, cached under test/fixtures/llm-agent-cache/agent/.
module Agent.Integration.WriterConversationHistorySpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import Runix.Logging (info)
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Core.Git (runStorage)
import Storyteller.Core.Runtime (Main)
import Storyteller.Writer.Agent (Instruction(..), Prompt(..), Prose(..))
import Storyteller.Writer.Agent.Write (writeAgent)

import Agent.Integration.Harness (Runner, runExpect)
import Agent.Integration.Judge (judgeOrFail)

scenePath :: FilePath
scenePath = "chapters/ch1.md"

turnOneInstruction :: T.Text
turnOneInstruction = T.unwords
  [ "Introduce a young clockmaker named Bram, alone at his workbench late"
  , "at night, carefully repairing a pocket watch. In passing, establish"
  , "that he is left-handed."
  ]

turnTwoInstruction :: T.Text
turnTwoInstruction = T.unwords
  [ "Continue: Bram sets down his tweezers and reaches for the soldering"
  , "iron to fuse a broken hairspring."
  ]

judgeQuestion :: T.Text
judgeQuestion = T.unwords
  [ "Bram was established as left-handed. Does this text show him reaching"
  , "for or holding the soldering iron with his left hand (or otherwise"
  , "acting in a way consistent with being left-handed), rather than his"
  , "right hand or being ambiguous/silent about which hand he uses? Answer"
  , "no if he uses his right hand, or if handedness isn't indicated at all."
  ]

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "same-file conversation history reaching the writer (real LLM, cached)" $
  it "keeps a second turn consistent with a detail only the first turn's own reply established" $
    runExpect @judgeModel runner $ do
      -- Turn one: gather this file's history so far (empty), generate, store
      -- the prompt, then persist the result as an atom -- exactly what turns
      -- this into a real prior turn for turn two to see. History has to be
      -- read *before* this turn's own prompt is stored -- see
      -- 'Server.Writer.File.chatWriter''s own Haddock on why storing first
      -- would make 'writeAgent' see this turn's own instruction twice (once
      -- via history, once as its trailing instruction message).
      historySoFar1 <- runStorage @Main (Tick.fileTicksOf scenePath)
      Prose turnOneText <- writeAgent [] [] [] [] [] historySoFar1 (Instruction turnOneInstruction)
      info ("turn one output:\n" <> turnOneText)
      embed $ turnOneText `shouldNotBe` ""
      _ <- runStorage @Main (Tick.storeAs (Prompt scenePath turnOneInstruction))
      _ <- runStorage @Main (Ops.append scenePath turnOneText)

      -- Turn two: same file, new prompt, history now includes turn one's
      -- own prompt/reply pair -- read before this turn's own prompt is
      -- stored, same discipline as turn one.
      historySoFar2 <- runStorage @Main (Tick.fileTicksOf scenePath)
      info $ "history ticks for turn two: " <> T.pack (show (length historySoFar2))
      embed $ length historySoFar2 `shouldSatisfy` (> length historySoFar1)

      Prose turnTwoText <- writeAgent [] [] [] [] [] historySoFar2 (Instruction turnTwoInstruction)
      info ("turn two output:\n" <> turnTwoText)
      embed $ turnTwoText `shouldNotBe` ""
      judgeOrFail @judgeModel turnTwoText judgeQuestion
