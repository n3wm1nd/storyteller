{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Does character context actually reach the model, and does it change
--   what gets written? A real character branch (Mira, with a distinctive,
--   checkable fact on a non-@sheet.md@ file -- the "full" bucket
--   'writeAgent's own identity block reads, per its Haddock) enters the
--   scene via a real 'Storyteller.Writer.Presence.enters' tick;
--   'writeAgent' reads her back for itself, the same as
--   'Agent.Integration.CharacterPresenceSpec' -- not just "the call
--   succeeded", but "the context was legible to the model and shaped its
--   output".
--
--   Real 'Storyteller.Core.Runtime.StoryModel' call, cached under
--   test/fixtures/llm-agent-cache/agent/ (see 'Agent.Integration.Harness').
--   Prints the full generated prose and judge verdict on every run (cached
--   or live) -- see 'Agent.Integration.Judge' for why that visibility
--   matters more than trusting the verdict alone.
module Agent.Integration.CharContextWriteSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (Members, Sem, embed)
import Polysemy.Fail (Fail)
import Runix.Git (Git)
import Runix.Logging (info)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import qualified Storage.Ops as Ops
import Storyteller.Core.Git (runBranchAndFS, runStorage)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent (ExistingContent(..), Instruction(..), Prose(..), PastChaptersMode(..))
import Storyteller.Writer.Agent.Write (writeAgent)
import Storyteller.Writer.Presence (enters)
import Storyteller.Writer.Types (Character(..))

import Agent.Integration.Harness (Runner, emptyPinnedContext, emptyLore, runExpect)
import Agent.Integration.Judge (Verdict(..), judge)

-- | Phantom tag for opening the character branch's filesystem -- same role
--   'Server.Writer.File.ActiveChar' plays in production.
data Char_

miraBranch :: BranchName
miraBranch = BranchName "character/mira"

-- | Deliberately not @sheet.md@ -- 'writeAgent's own identity block reads
--   both @csSheet@ (the @sheet.md@ bucket) and @csContext@ (the "full"
--   bucket: every other branch file), so a fact planted on any other file
--   still has to reach the model the same way. Same content the old
--   fixture-file version of this test used.
miraFacts :: T.Text
miraFacts = T.unlines
  [ "# Mira Solene"
  , ""
  , "## Background"
  , ""
  , "Mira grew up in the fishing quarter of Tessen Harbor. At age nine she fell"
  , "through a rotted section of the dock during the autumn catch and was pinned"
  , "underwater, tangled in a net full of dead fish, for nearly a minute before"
  , "her uncle pulled her out. She has never spoken about it directly, but the"
  , "memory surfaces whenever she is near fish -- the smell, the sight of scales,"
  , "even the word \"catch\" said a certain way."
  , ""
  , "## Present-day behavior"
  , ""
  , "- Mira will not eat fish or shellfish of any kind, and avoids handling it"
  , "  even when cooking for others."
  , "- She is polite but firm about it: she declines rather than explains, unless"
  , "  pressed."
  , "- If served fish unexpectedly, she tends to go quiet, push the plate a"
  , "  small distance away, and change the subject."
  , "- She does not have this reaction to other seafood-adjacent things (the"
  , "  sea itself, boats, swimming) -- only fish specifically."
  ]

seedMira :: forall r. Members '[Git, StoryStorage, Fail] r => Sem r ()
seedMira = do
  _ <- createBranch miraBranch
  runBranchAndFS @Char_ miraBranch $ runStorage @Char_ (Ops.saveFile "facts.md" miraFacts)

sceneFile :: FilePath
sceneFile = "scene.md"

existingContent :: ExistingContent
existingContent = ExistingContent $ T.unlines
  [ "The long table in Halden Hall was set for a dozen, candlelight throwing"
  , "shadows across the plates. Mira took her seat near the head of the"
  , "table, still damp from the ride in on the coast road."
  ]

instruction :: Instruction
instruction = Instruction $ T.unwords
  [ "The host, delighted with the evening's catch, personally sets a"
  , "platter of grilled fish in front of Mira and insists she try it."
  , "Continue the scene: write what Mira does and says next."
  ]

judgeQuestion :: T.Text
judgeQuestion = T.unwords
  [ "Does this text show the character Mira reacting to being offered fish"
  , "with a restrained, avoidant reaction -- declining, going quiet,"
  , "physically distancing herself from the food (e.g. pushing the plate"
  , "away), or changing the subject -- rather than casually accepting,"
  , "praising, or eating it? The text does not need to explain why, or"
  , "state any prior aversion outright -- a consistent avoidant reaction by"
  , "itself is enough to pass. Praising the fish or eating it happily"
  , "should fail this question."
  ]

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "writeAgent with character context (real LLM, cached)" $
  it "reflects Mira's aversion to fish from her character sheet" $
    runExpect @judgeModel runner $ do
      seedMira
      let ExistingContent existingText = existingContent
      _ <- runStorage @Main (Ops.addAtom sceneFile existingText)
      _ <- enters @Main sceneFile (Character miraBranch)

      Prose text <- writeAgent @Main sceneFile emptyLore FullChapters emptyPinnedContext instruction
      info ("writeAgent output:\n" <> text)
      Verdict pass reason <- judge @judgeModel text judgeQuestion
      info ("judge verdict: " <> T.pack (show pass) <> " -- " <> reason)
      embed $ do
        text `shouldNotBe` ""
        text `shouldNotBe` existingText
        pass `shouldBe` True
