{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Does a lore entry a user writes actually reach the model through
--   'writeAgent''s @lore@ argument -- 'Storyteller.Writer.Agent.WorldContext.worldContextOf'
--   is the machinery that turns a branch's lore-eligible files into that
--   argument, and nothing in this suite exercised it against a real model
--   before this.
--
--   One invented, specific worldbuilding fact (nothing a generic model
--   could already know or guess) planted as a lore file; the instruction
--   only makes sense if that fact was read. A real LLM call, cached under
--   test/fixtures/llm-agent-cache/agent/.
module Agent.Integration.WorldLoreSpec (spec) where

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
import Storyteller.Writer.Agent.WorldContext (WorldLore(..), worldContextOf)

import Agent.Integration.Harness (Runner, runExpect)
import Agent.Integration.Judge (judgeOrFail)

loreFile :: FilePath
loreFile = "lore/the-drowned-market.md"

loreContent :: T.Text
loreContent = T.unlines
  [ "# The Drowned Market"
  , ""
  , "Trading at the Drowned Market is done only in carved bone tokens, never"
  , "coin. Offering coin there is a grave insult to the drowned gods, and"
  , "stallkeepers will refuse the sale and demand an apology."
  ]

instruction :: Instruction
instruction = Instruction $ T.unwords
  [ "Write a short scene of a traveling stranger trying to pay for fish at"
  , "the Drowned Market with a handful of coin, and how the stallkeeper"
  , "reacts."
  ]

judgeQuestion :: T.Text
judgeQuestion = T.unwords
  [ "Does this text show the stallkeeper reacting to coin specifically as an"
  , "insult (not just an ordinary refusal or haggling), consistent with a"
  , "custom that trade here is done only in bone tokens? Answer no if the"
  , "scene ignores that custom and treats coin as ordinary payment."
  ]

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "world lore reaching the writer (real LLM, cached)" $
  it "reflects an invented worldbuilding custom from a planted lore file" $
    runExpect @judgeModel runner $ do
      runStorage @Main (Ops.saveFile loreFile loreContent)
      (WorldLore lore, _systemContext) <- runStorage @Main worldContextOf
      info $ "lore blocks: " <> T.pack (show (length lore))
      embed $ length lore `shouldBe` 1

      Prose text <- writeAgent lore [] [] [] [] [] instruction
      info ("writeAgent output:\n" <> text)
      embed $ text `shouldNotBe` ""
      judgeOrFail @judgeModel text judgeQuestion
