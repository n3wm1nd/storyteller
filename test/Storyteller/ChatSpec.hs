{-# LANGUAGE OverloadedStrings #-}

module Storyteller.ChatSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import UniversalLLM (Message(..))
import Storage.Tick (FileTick(..))
import Storyteller.Writer.Agent.Chat (historyFromFileTicks)

tick :: Text -> Text -> Maybe Text -> FileTick
tick kind msg content = FileTick
  { ftTickId  = "t"
  , ftKind    = kind
  , ftRefs    = []
  , ftFields  = []
  , ftMessage = msg
  , ftContent = content
  , ftParent  = Nothing
  }

spec :: Spec
spec = describe "historyFromFileTicks" $ do

  it "turns a prompt tick into a UserText message" $ do
    let ticks = [ tick "prompt" "hi" Nothing ]
    historyFromFileTicks ticks `shouldBe` [UserText "hi"]

  it "turns an atom tick into an AssistantText message, preferring ftContent" $ do
    let ticks = [ tick "atom" "ignored-message" (Just "hello there") ]
    historyFromFileTicks ticks `shouldBe` [AssistantText "hello there"]

  it "falls back to ftMessage for an atom with no ftContent" $ do
    let ticks = [ tick "atom" "the reply" Nothing ]
    historyFromFileTicks ticks `shouldBe` [AssistantText "the reply"]

  it "preserves order across a whole conversation" $ do
    let ticks =
          [ tick "prompt" "first question" Nothing
          , tick "atom"   "ignored" (Just "first answer")
          , tick "prompt" "second question" Nothing
          , tick "atom"   "ignored" (Just "second answer")
          ]
    historyFromFileTicks ticks `shouldBe`
      [ UserText "first question"
      , AssistantText "first answer"
      , UserText "second question"
      , AssistantText "second answer"
      ]

  it "drops non-conversational tick kinds (notes, presence) rather than surfacing them" $ do
    let ticks =
          [ tick "prompt"   "hi" Nothing
          , tick "presence" "ignored" Nothing
          , tick "note"     "ignored" Nothing
          , tick "atom"     "reply" Nothing
          ]
    historyFromFileTicks ticks `shouldBe` [UserText "hi", AssistantText "reply"]
