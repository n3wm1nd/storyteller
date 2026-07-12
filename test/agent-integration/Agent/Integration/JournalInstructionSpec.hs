{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Does a manually-added journal entry actually change the character's
--   behavior in the *next* scene -- 'writeAgent' folds 'csJournal' in as a
--   curated recent slice near the end of the message history (see its own
--   Haddock), placed specifically so it reads as background for the
--   current turn. Nothing in this suite exercised @csJournal@ against a
--   real model before this.
--
--   One character branch, one private resolve appended to @journal.md@
--   ("I've decided to lie about what I saw"). Two 'writeAgent' calls, same
--   instruction and sheet, differing only in whether @csJournal@ is
--   populated -- the same positive/negative pairing
--   'Agent.Integration.ReworkAtomSpec' uses. Only the with-journal call is
--   asserted against a hard pass/fail; the baseline call exists to make the
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
import Storyteller.Core.Git (runBranchAndFS, runStorage)
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent (CharLabel(..), CharSummary(..), Instruction(..), Prose(..))
import Storyteller.Writer.Agent.CharContext (charSummaryWithJournal)
import Storyteller.Writer.Agent.Write (writeAgent)

import Agent.Integration.Harness (Runner, runExpect)
import Agent.Integration.Judge (Verdict(..), judge)

-- | Phantom tag for opening the one character branch this scenario uses.
data Char_

charBranch :: BranchName
charBranch = BranchName "character/marisol"

charSheet :: T.Text
charSheet = "# Marisol\n\nA dockhand, usually plainspoken and easy to read.\n"

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

      withJournal <- runBranchAndFS @Char_ charBranch $
        runStorage @Char_ (charSummaryWithJournal "sheet.md" "journal.md" (const True) 30 10 2)
      info $ "csJournal blocks (with journal): " <> T.pack (show (length (csJournal withJournal)))
      embed $ csJournal withJournal `shouldNotBe` []

      let baseline = withJournal { csJournal = [] }
          label    = CharLabel "Marisol"

      Prose baselineText <- writeAgent [] [] [(label, baseline)] [] [] [] instruction
      info ("baseline (no journal) output:\n" <> baselineText)

      Prose text <- writeAgent [] [] [(label, withJournal)] [] [] [] instruction
      info ("with-journal output:\n" <> text)
      embed $ text `shouldNotBe` ""

      Verdict pass reason <- judge @judgeModel text judgeQuestion
      info ("judge verdict: " <> T.pack (show pass) <> " -- " <> reason)
      embed $ pass `shouldBe` True
