{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Does character context actually reach the model, and does it change
--   what gets written? 'charSummaryAgent' reads a character branch's files
--   and formats them as context blocks; this feeds those blocks into
--   'writeAgent' alongside an instruction that only makes sense if a
--   specific fact from the character sheet (test/fixtures/agent-integration/
--   char-context-write/character/mira/facts.md) was actually read and used
--   -- not just "the call succeeded", but "the context was legible to the
--   model and shaped its output".
--
--   Reading the fixture is a plain 'IO' step, done *before* the scenario is
--   handed to the shared 'Runner' (via 'readCharFixture', which runs its
--   own tiny, throwaway 'FileSystem' stack) rather than inside the 'Sem'
--   action -- the 'Runner' passed in (built once by @Main.hs@) has no
--   filesystem effect at all, so there's nothing for this spec to layer
--   onto in the first place. 'renderCharContext' (the pure half of
--   'charSummaryAgent') turns the plain @(path, content)@ pairs read here
--   into the same 'CharContextBlock's the effectful version would have
--   produced.
--
--   Real 'Storyteller.Core.Runtime.StoryModel' call, cached under
--   test/fixtures/llm-agent-cache/agent/ (see 'Agent.Integration.Harness').
--   Prints the full generated prose and judge verdict on every run (cached
--   or live) -- see 'Agent.Integration.Judge' for why that visibility
--   matters more than trusting the verdict alone.
module Agent.Integration.CharContextWriteSpec (spec) where

import Prelude hiding (readFile)

import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed, runM)
import Polysemy.Fail (runFail)
import Runix.FileSystem (HasProjectPath(..), fileSystemLocal)
import Runix.FileSystem.System (filesystemIO)

import Runix.Logging (info)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)
import Storyteller.Writer.Agent (CharContextBlock, CharLabel(..), CharSummary(..), ExistingContent(..), Instruction(..), Prose(..))
import Storyteller.Writer.Agent.CharContext (readCharFiles, renderCharContext)
import Storyteller.Writer.Agent.Write (writeAgent)

import Agent.Integration.Harness (Runner, resolveFixture, runExpect)
import Agent.Integration.Judge (Verdict(..), judge)

-- | Chroot marker for the character-branch fixture directory, used only
--   inside 'readCharFixture''s own standalone stack -- never in scope for
--   a 'Runner''s @action@.
newtype CharFixture = CharFixture FilePath

instance HasProjectPath CharFixture where
  getProjectPath (CharFixture p) = p

-- | Read and render the character fixture as plain 'IO', independent of
--   the LLM-effect stack a 'Runner' wraps.
readCharFixture :: FilePath -> IO [CharContextBlock]
readCharFixture dir = do
  result <- runM . runFail . filesystemIO . fileSystemLocal (CharFixture dir) $ readCharFiles @CharFixture (const True)
  either (fail . ("readCharFixture: " <>)) (pure . renderCharContext) result

charFixtureDir :: FilePath
charFixtureDir = "test/fixtures/agent-integration/char-context-write/character"

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
  it "reflects Mira's aversion to fish from her character sheet" $ do
    resolvedCharDir <- resolveFixture charFixtureDir
    charBlocks      <- readCharFixture resolvedCharDir

    let ExistingContent existingText = existingContent
        charSummary = CharSummary { csSheet = [], csContext = charBlocks, csJournal = [] }

    runExpect @judgeModel runner $ do
      Prose text <- writeAgent [] [] [(CharLabel "Mira", charSummary)] [] [("scene.md", existingText)] [] instruction
      info ("writeAgent output:\n" <> text)
      Verdict pass reason <- judge @judgeModel text judgeQuestion
      info ("judge verdict: " <> T.pack (show pass) <> " -- " <> reason)
      embed $ do
        text `shouldNotBe` ""
        text `shouldNotBe` existingText
        pass `shouldBe` True
