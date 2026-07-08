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

import Polysemy (runM)
import Polysemy.Fail (runFail)
import Runix.FileSystem (HasProjectPath(..), fileSystemLocal)
import Runix.FileSystem.System (filesystemIO)

import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)
import Storyteller.Writer.Agent (CharContextBlock, CharLabel(..), ExistingContent(..), Instruction(..), Prose(..))
import Storyteller.Writer.Agent.CharContext (readCharFiles, renderCharContext)
import Storyteller.Writer.Agent.Write (writeAgent)

import Agent.Integration.Harness (Runner, resolveFixture)
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
  result <- runM . runFail . filesystemIO . fileSystemLocal (CharFixture dir) $ readCharFiles @CharFixture
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
  [ "Does this text show the character Mira declining, avoiding, or"
  , "reacting negatively to being offered fish, in a way consistent with"
  , "a stated aversion to fish in her background? Praising the fish or"
  , "eating it happily should fail this question."
  ]

spec
  :: forall storyModel judgeModel
  .  ( SupportsSystemPrompt (ProviderOf storyModel)
     , HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel) )
  => Runner storyModel judgeModel -> Spec
spec runner = describe "writeAgent with character context (real LLM, cached)" $
  it "reflects Mira's aversion to fish from her character sheet" $ do
    resolvedCharDir <- resolveFixture charFixtureDir
    charBlocks      <- readCharFixture resolvedCharDir

    -- Empty config list: the model's own defaults (from
    -- 'Agent.Integration.Harness.knownModels') already came baked into
    -- the interpreter 'runner' wraps, so there's nothing to add per-call
    -- here.
    result <- runner $ do
      prose@(Prose text) <- writeAgent @storyModel [] existingContent [] instruction [(CharLabel "Mira", charBlocks)]
      verdict <- judge @judgeModel text judgeQuestion
      pure (prose, verdict)

    case result of
      Left err -> expectationFailure err
      Right (Prose text, Verdict pass reason) -> do
        putStrLn "\n=== writeAgent output ==="
        putStrLn (T.unpack text)
        putStrLn "\n=== judge verdict ==="
        putStrLn (show pass <> " -- " <> T.unpack reason)

        let ExistingContent existingText = existingContent
        text `shouldNotBe` ""
        text `shouldNotBe` existingText
        pass `shouldBe` True
