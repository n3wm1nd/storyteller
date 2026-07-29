{-# LANGUAGE OverloadedStrings #-}

module Storyteller.Writer.ConversationSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Storage.Tick (FileTick(..))
import Storyteller.Writer.Conversation (Turn(..), turnsFromFileTicks)

-- | A minimal atom 'FileTick' -- only the fields 'turnsFromFileTicks'
--   itself actually reads ('ftKind', 'ftContent', 'ftHidden' via
--   'ftFields') are meaningful here.
atomTick :: T.Text -> FileTick
atomTick content = FileTick
  { ftTickId  = "t"
  , ftKind    = "atom"
  , ftRefs    = []
  , ftFields  = []
  , ftMessage = content
  , ftContent = Just content
  , ftParent  = Nothing
  }

promptTick :: T.Text -> FileTick
promptTick msg = (atomTick msg) { ftKind = "prompt", ftContent = Nothing }

spec :: Spec
spec = describe "turnsFromFileTicks" $ do
  it "strips an assistant turn's trailing paragraph-boundary newlines" $
    turnsFromFileTicks [atomTick "The door creaked open.\n\n"]
      `shouldBe` [AssistantTurn "The door creaked open."]

  it "strips a run of several trailing newlines, not just one" $
    turnsFromFileTicks [atomTick "She stepped inside.\n\n\n\n"]
      `shouldBe` [AssistantTurn "She stepped inside."]

  it "leaves an assistant turn with no trailing newline untouched" $
    turnsFromFileTicks [atomTick "The last line of the chapter."]
      `shouldBe` [AssistantTurn "The last line of the chapter."]

  it "preserves internal blank lines between paragraphs within one atom" $
    turnsFromFileTicks [atomTick "First paragraph.\n\nSecond paragraph.\n\n"]
      `shouldBe` [AssistantTurn "First paragraph.\n\nSecond paragraph."]

  it "never trims a user (prompt) turn -- only assistant turns carry split artifacts" $
    turnsFromFileTicks [promptTick "Write the next scene.\n\n"]
      `shouldBe` [UserTurn "Write the next scene.\n\n"]
