{-# LANGUAGE OverloadedStrings #-}

module Storyteller.SplitterSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Storyteller.Agent.Splitter (byParagraph)

spec :: Spec
spec = do
  describe "byParagraph" $ do
    it "single paragraph" $
      byParagraph "hello world" `shouldBe` ["hello world"]

    it "two paragraphs: delimiter appended to first atom" $
      byParagraph "first\n\nsecond" `shouldBe` ["first\n\n", "second"]

    it "three paragraphs" $
      byParagraph "a\n\nb\n\nc" `shouldBe` ["a\n\n", "b\n\n", "c"]

    it "preserves internal newlines within a paragraph" $
      byParagraph "line one\nline two\n\nnext para"
        `shouldBe` ["line one\nline two\n\n", "next para"]

    it "preserves leading whitespace within a line" $
      byParagraph "  indented\n\nnormal" `shouldBe` ["  indented\n\n", "normal"]

    it "extra newlines stay in the atom" $
      byParagraph "a\n\n\n\nb" `shouldBe` ["a\n\n\n\n", "b"]

    it "leading blank lines go with the empty first atom" $
      byParagraph "\n\nfirst" `shouldBe` ["\n\n", "first"]

    it "trailing blank lines stay in the preceding atom, no empty tail" $
      byParagraph "last\n\n" `shouldBe` ["last\n\n"]

    it "empty string gives no atoms" $
      byParagraph "" `shouldBe` []

  describe "byParagraph / QuickCheck" $ do
    it "is lossless: concat of atoms equals the original" $
      property $ \s ->
        let t = T.pack s
        in T.concat (byParagraph t) === t

