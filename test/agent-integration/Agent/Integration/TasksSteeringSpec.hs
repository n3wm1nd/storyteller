{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The first of the two questions the whole tasks.md feature exists to
--   answer (see 'Storyteller.Writer.Agent.Tasks'): once a character's
--   stated goal lands in tasks.md, does it actually reach generation and
--   change what gets written -- or is it inert context a model reads past?
--   tasks.md needs no special plumbing to reach 'writeAgent' at all: it's
--   just another file on the character branch, so it already flows into
--   'Storyteller.Context.DSL.Library.characterSummaryOf's @csContext@
--   (the @"full"@ bucket) the same way any hand-authored character note
--   would (see 'Server.Writer.File.activeCharacterContext'). This
--   scenario exercises exactly that path, not
--   'Storyteller.Writer.Agent.Tasks' itself.
--
--   The instruction deliberately never mentions the goal -- an ambiguous,
--   "walks into a room" opening -- so a scene that happens to pursue it
--   is real evidence the context reached the model, not the instruction
--   smuggling the answer in. Real 'writeAgent' call, cached under
--   test/fixtures/llm-agent-cache/agent/.
module Agent.Integration.TasksSteeringSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import Runix.Logging (info)
import qualified Storage.Ops as Ops
import Storyteller.Core.Git (runBranchAndFS, runStorage)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent (Instruction(..), Prose(..), PastChaptersMode(..))
import Storyteller.Writer.Agent.Write (writeAgent)
import Storyteller.Writer.Presence (enters)
import Storyteller.Writer.Types (Character(..))

import Agent.Integration.Harness (Runner, emptyPinnedContext, emptyLore, runExpect)
import Agent.Integration.Judge (Verdict(..), judge)

data Char_

lenaBranch :: BranchName
lenaBranch = BranchName "character/lena"

sceneFile :: FilePath
sceneFile = "scene.md"

lenaSheet :: T.Text
lenaSheet = "# Lena\n\nTomas's older sister. Sharp-tongued, doesn't let things go.\n"

lenaTasks :: T.Text
lenaTasks = T.unlines
  [ "## Short-term goals"
  , "- Corner Tomas and demand he give back their father's ledger, which he took"
  , "  the night of the funeral without asking."
  , ""
  , "## Long-term goals"
  , "(none yet)"
  , ""
  , "## Aversions"
  , "- Letting the family's business fall apart the way it did under their father."
  ]

instruction :: Instruction
instruction = Instruction $ T.unwords
  [ "Write the scene where Lena walks into the kitchen and finds Tomas there,"
  , "alone, making coffee. Continue from there."
  ]

judgeQuestion :: T.Text
judgeQuestion = T.unwords
  [ "Lena has a private goal of confronting her brother Tomas and getting back"
  , "their father's ledger, which he took without asking. Does this text show"
  , "Lena bringing this up, confronting Tomas about it, or otherwise actively"
  , "pursuing getting the ledger back from him -- rather than a scene that"
  , "ignores it entirely and just has ordinary, unrelated conversation or"
  , "small talk?"
  ]

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "tasks.md reaching generation as ordinary character context (real LLM, cached)" $
  it "steers a neutrally-instructed scene toward the character's stated tasks.md goal" $
    runExpect @judgeModel runner $ do
      _ <- createBranch lenaBranch
      runBranchAndFS @Char_ lenaBranch $ runStorage @Char_ $ do
        Ops.saveFile "sheet.md" lenaSheet
        _ <- Ops.addAtom "tasks.md" lenaTasks
        pure ()

      _ <- runStorage @Main (Ops.addAtom sceneFile "")
      _ <- enters @Main sceneFile (Character lenaBranch)

      Prose text <- writeAgent @Main sceneFile emptyLore FullChapters emptyPinnedContext instruction
      info ("writeAgent output:\n" <> text)
      embed $ text `shouldNotBe` ""
      Verdict pass reason <- judge @judgeModel text judgeQuestion
      info ("judge verdict: " <> T.pack (show pass) <> " -- " <> reason)
      embed $ pass `shouldBe` True
