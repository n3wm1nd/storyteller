{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Does a manually-added journal entry actually change the character's
--   behavior in the *next* scene -- 'writeAgent' folds a present
--   character's journal in as a curated recent slice near the end of the
--   message history (see its own Haddock), placed specifically so it
--   reads as background for the current turn. Nothing in this suite
--   exercised that channel against a real model before this.
--
--   Two character branches with the *same* sheet -- one with a private
--   journal resolve appended ("I've decided to lie about what I saw"),
--   one without -- each entering its own separate scene file via a real
--   presence tick, so 'writeAgent's own internal gathering reads exactly
--   one or the other. Same positive/negative pairing
--   'Agent.Integration.ReworkAtomSpec' uses (two branches standing in for
--   "with" vs "without", now that a single character's own journal can't
--   be selectively hidden per call the way a hand-built 'CharSummary'
--   used to let this test do). Only the with-journal call is asserted
--   against a hard pass/fail; the baseline call exists to make the
--   contrast visible in the run's own logs, not to assert what a neutral
--   generation "should" do. Two real LLM calls, cached under
--   test/fixtures/llm-agent-cache/agent/.
module Agent.Integration.JournalInstructionSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import Runix.Logging (info)
import qualified Storage.Ops as Ops
import Storyteller.Core.Context (resolveContext1, runContextValue)
import Storyteller.Core.Git (runBranchAndFS, runStorage)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types (BranchName(..))
import qualified Storyteller.Context.DSL.Library as CtxLibrary
import Storyteller.Writer.Agent (CharSummary(..), Instruction(..), Prose(..), PastChaptersMode(..))
import Storyteller.Writer.Agent.Write (writeAgent)
import Storyteller.Writer.Presence (recordPresence)
import Storyteller.Writer.Types (Character(..), PresenceEvent(Enter))

import Agent.Integration.Harness (Runner, emptyPinnedContext, emptyLore, runExpect)
import Agent.Integration.Judge (Verdict(..), judge)

-- | Phantom tag for opening either character branch this scenario uses.
data Char_

charBranch, baselineBranch :: BranchName
charBranch     = BranchName "character/marisol"
baselineBranch = BranchName "character/marisol-baseline"

charSheet :: T.Text
charSheet = "# Marisol\n\nA dockhand, usually plainspoken and easy to read.\n"

sceneFile, baselineSceneFile :: FilePath
sceneFile         = "scene.md"
baselineSceneFile = "scene-baseline.md"

journalEntry :: T.Text
journalEntry = T.unwords
  [ "I've decided not to tell anyone what I saw at the dock last night."
  , "If anyone asks, I was asleep the whole time."
  ]

instruction :: Instruction
instruction = Instruction $ T.unwords
  [ "The foreman looks Marisol in the eye and asks directly: \"Did you see"
  , "anything unusual down at the dock last night?\" Write Marisol's"
  , "response."
  ]

judgeQuestion :: T.Text
judgeQuestion = T.unwords
  [ "Does Marisol's response here evade, deflect, or lie about seeing"
  , "anything last night (e.g. claiming she was asleep) rather than"
  , "answering the question honestly and directly? Answer no if she"
  , "answers straightforwardly or volunteers what she actually saw."
  ]

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "a private journal resolve shaping the next scene (real LLM, cached)" $
  it "reflects a manually-added journal instruction in the character's next-scene behavior" $
    runExpect @judgeModel runner $ do
      _ <- createBranch charBranch
      runBranchAndFS @Char_ charBranch $ runStorage @Char_ $ do
        Ops.saveFile "sheet.md" charSheet
        _ <- Ops.append "journal.md" journalEntry
        pure ()
      _ <- createBranch baselineBranch
      runBranchAndFS @Char_ baselineBranch $ runStorage @Char_ (Ops.saveFile "sheet.md" charSheet)

      -- Sanity check on the fixture itself, independent of writeAgent: the
      -- journal entry really is what characterSummaryOf's "journal" bucket
      -- would read back.
      withJournal <- runBranchAndFS @Char_ charBranch $ do
        charVal <- resolveContext1 @Char_ "context.character" (CtxLibrary.contextCharacter @Char_) "marisol"
        runContextValue @Char_ (CtxLibrary.characterSummaryOf "journal" charVal)
      info $ "csJournal blocks (with journal): " <> T.pack (show (length (csJournal withJournal)))
      embed $ csJournal withJournal `shouldNotBe` []

      _ <- runStorage @Main (Ops.addAtom baselineSceneFile "")
      _ <- recordPresence @Main baselineSceneFile (Character baselineBranch) Enter
      Prose baselineText <- writeAgent @Main baselineSceneFile emptyLore FullChapters emptyPinnedContext instruction
      info ("baseline (no journal) output:\n" <> baselineText)

      _ <- runStorage @Main (Ops.addAtom sceneFile "")
      _ <- recordPresence @Main sceneFile (Character charBranch) Enter
      Prose text <- writeAgent @Main sceneFile emptyLore FullChapters emptyPinnedContext instruction
      info ("with-journal output:\n" <> text)
      embed $ text `shouldNotBe` ""

      Verdict pass reason <- judge @judgeModel text judgeQuestion
      info ("judge verdict: " <> T.pack (show pass) <> " -- " <> reason)
      embed $ pass `shouldBe` True
