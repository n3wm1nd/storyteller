{-# LANGUAGE OverloadedStrings #-}

module Storyteller.Writer.ReplaceToolSpec (spec) where

import Test.Hspec

import Storyteller.Writer.Agent.ReplaceTool (replaceOnce)

spec :: Spec
spec = describe "replaceOnce" $ do
  it "replaces the one occurrence of old text with new text" $
    replaceOnce "blue" "green" "Her eyes were a deep blue, like the sea."
      `shouldBe` Just "Her eyes were a deep green, like the sea."

  it "refuses when old text doesn't appear at all" $
    replaceOnce "purple" "green" "Her eyes were a deep blue."
      `shouldBe` Nothing

  it "refuses when old text is ambiguous (appears more than once)" $
    replaceOnce "the" "a" "the cat sat on the mat"
      `shouldBe` Nothing

  it "refuses an empty old text" $
    replaceOnce "" "green" "Her eyes were a deep blue."
      `shouldBe` Nothing

  it "leaves the rest of the text untouched, not just the matched span" $
    replaceOnce "blue" "green" "Her eyes were a deep blue, the same shade as her mother's."
      `shouldBe` Just "Her eyes were a deep green, the same shade as her mother's."
