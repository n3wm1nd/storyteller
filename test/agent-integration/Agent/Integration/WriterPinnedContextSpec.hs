{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Does 'writeAgent''s @pinned@ argument -- the user's own short-term
--   selection, per its Haddock, distinct from @lore@ (branch files) and
--   @csJournal@ (a character's own material) -- actually reach the model?
--   Nothing plants this as a file anywhere: unlike lore or a journal entry,
--   a pinned item is ephemeral, assembled by the caller for one call (see
--   'Server.Writer.File.chatWriter''s own @context@ parameter, folded into
--   @pinned@ alongside the target file's own other-file context) rather
--   than read off a branch, so this scenario builds the 'ContextBlock'
--   directly instead of writing anything to storage first.
--
--   One planted scene-state fact a generic model has no way to already
--   know (a sudden power outage), on an instruction that doesn't repeat it
--   -- the same "one channel isolated, nothing else could have supplied
--   this" shape as 'Agent.Integration.WorldLoreSpec'. A real LLM call,
--   cached under test/fixtures/llm-agent-cache/agent/.
module Agent.Integration.WriterPinnedContextSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import Runix.Logging (info)
import Storyteller.Writer.Agent (ContextBlock(..), Instruction(..), Prose(..))
import Storyteller.Writer.Agent.Write (writeAgent)

import Agent.Integration.Harness (Runner, runExpect)
import Agent.Integration.Judge (judgeOrFail)

pinned :: [ContextBlock]
pinned =
  [ ContextBlock $ T.unwords
      [ "Current scene conditions: a power outage has just plunged the"
      , "entire building into total darkness -- every light, everywhere,"
      , "is out, with no sign of coming back on soon."
      ]
  ]

instruction :: Instruction
instruction = Instruction $ T.unwords
  [ "Continue the scene: write the moment Talia gets up from the sofa and"
  , "tries to make her way to the kitchen to find a flashlight."
  ]

judgeQuestion :: T.Text
judgeQuestion = T.unwords
  [ "Does this text show Talia navigating in complete darkness -- feeling"
  , "her way, groping for walls or furniture, unable to simply see where"
  , "she's going -- rather than moving normally as if the lights were on?"
  , "Answer no if the scene doesn't acknowledge the darkness at all, or has"
  , "her navigate as if she could see normally."
  ]

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "pinned/short-term context reaching the writer (real LLM, cached)" $
  it "reflects a planted pinned scene-state fact that the instruction never repeats" $
    runExpect @judgeModel runner $ do
      Prose text <- writeAgent [] [] [] pinned [] [] instruction
      info ("writeAgent output:\n" <> text)
      embed $ text `shouldNotBe` ""
      judgeOrFail @judgeModel text judgeQuestion
