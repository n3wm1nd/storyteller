{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Storyteller.Writer.WriteSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import UniversalLLM (Message(..))
import Storage.Tick (FileTick(..))

import Storyteller.Core.LLM.Role (ProseModel)
import Storyteller.Writer.Agent (Instruction(..), ContextBlock(..), CharContextBlock(..), CharLabel(..), CharSummary(..))
import Storyteller.Writer.Agent.Write (buildChapterMessages)

-- | A bare-minimum atom tick: only the fields 'Storyteller.Writer.Agent.
--   Chat.historyFromFileTicks' actually reads (@ftKind@, @ftMessage@,
--   @ftContent@) are meaningful here.
atomTick :: T.Text -> FileTick
atomTick msg = FileTick
  { ftTickId = "atom", ftKind = "atom", ftRefs = [], ftFields = []
  , ftMessage = msg, ftContent = Just msg, ftParent = Nothing
  }

promptTick :: T.Text -> FileTick
promptTick msg = FileTick
  { ftTickId = "prompt", ftKind = "prompt", ftRefs = [], ftFields = []
  , ftMessage = msg, ftContent = Nothing, ftParent = Nothing
  }

noSummary :: CharSummary
noSummary = CharSummary [] [] []

build
  :: [ContextBlock] -> [(CharLabel, CharSummary)] -> [ContextBlock] -> [T.Text] -> [FileTick] -> Instruction
  -> [Message ProseModel]
build = buildChapterMessages

spec :: Spec
spec = describe "buildChapterMessages" $ do

  it "with nothing else gathered, is just the instruction" $ do
    build [] [] [] [] [] (Instruction "continue the scene")
      `shouldBe` [UserText "## Instruction\n\ncontinue the scene\n\nWrite approximately 300 words.\nWrite only the new text to append. Do not repeat or summarise existing content."]

  it "puts world lore first, ahead of everything else" $ do
    let msgs = build [ContextBlock "### notes/tavern.md\n\nA tavern."] [] [] [] [] (Instruction "go")
    case msgs of
      (m : _) -> m `shouldBe` UserText "### notes/tavern.md\n\nA tavern."
      []      -> expectationFailure "expected at least one message"

  it "puts earlier chapters right after world lore, oldest first, one message each" $ do
    let msgs = build [] [] [] ["chapter one prose", "chapter two prose"] [] (Instruction "go")
    take 2 msgs `shouldBe` [UserText "chapter one prose", UserText "chapter two prose"]

  it "puts a character's sheet/context as one chapter-start message, journal excluded from it" $ do
    let alice = CharSummary
          { csSheet   = [CharContextBlock "### sheet.md\n\n# Alice"]
          , csContext = [CharContextBlock "### notes.md\n\nsome context"]
          , csJournal = [CharContextBlock "### journal excerpt\n\nsecret diary entry"]
          }
        msgs = build [] [(CharLabel "Alice", alice)] [] [] [] (Instruction "go")
    case msgs of
      (UserText t : _) -> do
        t `shouldSatisfy` (\txt -> "## Character: Alice" `isInfixOfText` txt)
        t `shouldSatisfy` (\txt -> "# Alice" `isInfixOfText` txt)
        t `shouldSatisfy` (\txt -> "some context" `isInfixOfText` txt)
        t `shouldSatisfy` (\txt -> not ("secret diary entry" `isInfixOfText` txt))
      other -> expectationFailure ("expected a UserText chapter-start message first, got " <> show other)

  it "reconstructs the current chapter's own history as alternating turns, in order" $ do
    let ticks = [promptTick "write the opening", atomTick "Once upon a time...", promptTick "now the twist", atomTick "...and then everything changed."]
        msgs  = build [] [] [] [] ticks (Instruction "go")
    take 4 msgs `shouldBe`
      [ UserText "write the opening"
      , AssistantText "Once upon a time..."
      , UserText "now the twist"
      , AssistantText "...and then everything changed."
      ]

  it "places the journal excerpt in its own shallow splice message, directly before the instruction -- not inside it, not at chapter-start" $ do
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [CharContextBlock "### journal\n\nshe remembers the storm"] }
        msgs  = build [] [(CharLabel "Alice", alice)] [] [] [] (Instruction "continue")
    length msgs `shouldBe` 2
    case msgs of
      [splice, instr] -> do
        splice `shouldBe` UserText "## Character: Alice\n\n### journal\n\nshe remembers the storm"
        case instr of
          UserText t -> t `shouldSatisfy` (\txt -> "## Instruction" `isInfixOfText` txt)
          other      -> expectationFailure ("expected the instruction message, got " <> show other)
      other -> expectationFailure ("expected exactly [splice, instruction], got " <> show other)

  it "merges pinned context and journal excerpts into the same single splice message" $ do
    let alice = CharSummary { csSheet = [], csContext = [], csJournal = [CharContextBlock "### journal\n\nshe remembers the storm"] }
        msgs  = build [] [(CharLabel "Alice", alice)] [ContextBlock "### pinned.md\n\nuser-pinned note"] [] [] (Instruction "continue")
    length msgs `shouldBe` 2
    case msgs of
      (UserText t : _) -> do
        t `shouldSatisfy` (\txt -> "user-pinned note" `isInfixOfText` txt)
        t `shouldSatisfy` (\txt -> "she remembers the storm" `isInfixOfText` txt)
      other -> expectationFailure ("expected the merged splice message first, got " <> show other)

  it "the instruction is always the last message, regardless of what else is present" $ do
    let alice = CharSummary
          { csSheet = [CharContextBlock "### sheet.md\n\n# Alice"], csContext = [], csJournal = [CharContextBlock "### journal\n\nnote"] }
        msgs = build
          [ContextBlock "### lore.md\n\nlore"]
          [(CharLabel "Alice", alice)]
          [ContextBlock "### pinned.md\n\npinned"]
          ["earlier chapter"]
          [atomTick "existing prose"]
          (Instruction "finish the scene")
    case reverse msgs of
      (UserText t : _) -> t `shouldSatisfy` (\txt -> "## Instruction" `isInfixOfText` txt && "finish the scene" `isInfixOfText` txt)
      other             -> expectationFailure ("expected the instruction as the last message, got " <> show (reverse other))

  it "drops empty sections instead of emitting empty messages" $ do
    build [] [(CharLabel "Alice", noSummary)] [] [] [] (Instruction "go")
      `shouldBe` [UserText "## Instruction\n\ngo\n\nWrite approximately 300 words.\nWrite only the new text to append. Do not repeat or summarise existing content."]
  where
    isInfixOfText needle haystack = needle `T.isInfixOf` haystack
