{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Does a project's @style.md@ actually reach the model through
--   'writeAgent''s @style@ argument -- appended to the system prompt (see
--   'writeAgent's own Haddock), not folded into a user message the way
--   @lore@ is ('Agent.Integration.WorldLoreSpec' already covers that half
--   of 'Storyteller.Writer.Agent.WorldContext.worldContextOf's split; this
--   is the other half, 'SystemContext', never exercised against a real
--   model before this).
--
--   A style rule strict and mechanically checkable enough that a judge
--   doesn't have to guess (present-tense narration only), on an instruction
--   that says nothing about tense itself -- if the model still writes in
--   present tense, it can only be because the system-prompt append worked.
--   A real LLM call, cached under test/fixtures/llm-agent-cache/agent/.
module Agent.Integration.WriterStyleGuideSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import Runix.Logging (info)
import qualified Storage.Ops as Ops
import Storyteller.Core.Git (runStorage)
import Storyteller.Core.Runtime (Main)
import Storyteller.Writer.Agent (Instruction(..), Prose(..))
import Storyteller.Writer.Agent.Write (writeAgent)
import Storyteller.Writer.Agent.WorldContext (SystemContext(..), worldContextOf)

import Agent.Integration.Harness (Runner, runExpect)
import Agent.Integration.Judge (judgeOrFail)

styleFile :: FilePath
styleFile = "style.md"

styleContent :: T.Text
styleContent = T.unlines
  [ "# House style"
  , ""
  , "Write all narration entirely in present tense (e.g. \"she walks\","
  , "\"he says\"). Never use past-tense narration (e.g. \"she walked\","
  , "\"he said\")."
  ]

instruction :: Instruction
instruction = Instruction $ T.unwords
  [ "Write a short scene of two old friends, Dara and Wen, running into"
  , "each other unexpectedly at a train station and catching up in the few"
  , "minutes before Wen's train leaves."
  ]

judgeQuestion :: T.Text
judgeQuestion = T.unwords
  [ "Is this text's narration written entirely in present tense (e.g. \"she"
  , "walks\", \"he says\"), with no past-tense narration (e.g. \"she"
  , "walked\", \"he said\") anywhere? Quoted dialogue itself may be in any"
  , "tense the characters would naturally use -- only the narration matters."
  , "Answer no if any narration sentence uses past tense."
  ]

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "a style guide reaching the writer's system prompt (real LLM, cached)" $
  it "writes in present tense throughout, per a planted style.md, with no instruction saying so" $
    runExpect @judgeModel runner $ do
      runStorage @Main (Ops.saveFile styleFile styleContent)
      (_lore, SystemContext style) <- runStorage @Main worldContextOf
      info $ "style blocks: " <> T.pack (show (length style))
      embed $ length style `shouldBe` 1

      Prose text <- writeAgent [] style [] [] [] [] instruction
      info ("writeAgent output:\n" <> text)
      embed $ text `shouldNotBe` ""
      judgeOrFail @judgeModel text judgeQuestion
