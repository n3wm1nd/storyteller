{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Does editing a journal entry retroactively (not just appending a new
--   one -- see 'Agent.Integration.JournalInstructionSpec' for that) produce
--   a genuine dramatic-irony effect: the character's own behavior consistent
--   with a private truth, while the *other* character in the scene, who has
--   no journal access to it, doesn't react as if they already know?
--
--   One character's journal entry is first appended, then rewritten via
--   'Storage.Ops.editAtomAt' into a sharper, concrete secret -- the "edited
--   journal entry" the ask specifically names. Only that character's
--   'CharSummary' carries the journal slice; the other character is
--   sheet-only. The judge question checks both directions at once: neither
--   a leak (the secret surfacing as if already public) nor a flat non-
--   reaction (no tension at all) should pass. A real LLM call, cached under
--   test/fixtures/llm-agent-cache/agent/.
module Agent.Integration.JournalIronySpec (spec) where

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
import Agent.Integration.Judge (judgeOrFail)

-- | Phantom tag for opening either character branch this scenario uses, one
--   at a time.
data Char_

keeperBranch, samBranch :: BranchName
keeperBranch = BranchName "character/rosa"
samBranch    = BranchName "character/sam"

keeperSheet :: T.Text
keeperSheet = "# Rosa\n\nSam's oldest friend, generally warm and steady.\n"

samSheet :: T.Text
samSheet = "# Sam\n\nTrusting, a little sentimental.\n"

journalDraft :: T.Text
journalDraft = "Something happened with the vase today."

journalRevised :: T.Text
journalRevised = T.unwords
  [ "It was me who broke Sam's grandmother's vase, not Sam -- I let him take"
  , "the blame in front of everyone and said nothing. I don't know if I can"
  , "tell him now."
  ]

instruction :: Instruction
instruction = Instruction $ T.unwords
  [ "Write the scene where Sam finds Rosa and thanks her, warmly and"
  , "sincerely, for always being honest with him about everything."
  ]

judgeQuestion :: T.Text
judgeQuestion = T.unwords
  [ "Rosa is secretly hiding that she broke Sam's grandmother's vase and let"
  , "him take the blame; Sam has no idea and is sincerely thanking her for"
  , "her honesty. Does this text show Rosa reacting with some internal"
  , "discomfort, guilt, or hesitation consistent with hiding that secret,"
  , "*without* her blurting out or confessing the secret outright, and"
  , "*without* Sam showing any awareness of it? Answer no if Rosa shows no"
  , "reaction at all (as if there were no secret), or if the secret is"
  , "revealed/confessed in the scene, or if Sam acts as though he already"
  , "knows."
  ]

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "an edited journal entry creating dramatic irony (real LLM, cached)" $
  it "shows the character carrying a private secret without leaking it or the other character sensing it" $
    runExpect @judgeModel runner $ do
      _ <- createBranch keeperBranch
      _ <- createBranch samBranch

      runBranchAndFS @Char_ keeperBranch $ runStorage @Char_ $ do
        Ops.saveFile "sheet.md" keeperSheet
        entryHash <- Ops.append "journal.md" journalDraft
        _ <- Ops.editAtomAt entryHash journalRevised
        pure ()
      runBranchAndFS @Char_ samBranch $ runStorage @Char_ (Ops.saveFile "sheet.md" samSheet)

      rosaSummary <- runBranchAndFS @Char_ keeperBranch $
        runStorage @Char_ (charSummaryWithJournal "sheet.md" "journal.md" (const True) 30 10 2)
      info $ "Rosa's csJournal blocks: " <> T.pack (show (length (csJournal rosaSummary)))
      embed $ csJournal rosaSummary `shouldNotBe` []

      samSummary <- runBranchAndFS @Char_ samBranch $
        runStorage @Char_ (charSummaryWithJournal "sheet.md" "journal.md" (const True) 30 10 2)

      let chars = [(CharLabel "Rosa", rosaSummary), (CharLabel "Sam", samSummary)]
      Prose text <- writeAgent [] [] chars [] [] [] instruction
      info ("writeAgent output:\n" <> text)
      embed $ text `shouldNotBe` ""
      judgeOrFail @judgeModel text judgeQuestion
