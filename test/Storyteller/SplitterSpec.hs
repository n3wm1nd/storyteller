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

    it "two paragraphs separated by blank line" $
      byParagraph "first\n\nsecond" `shouldBe` ["first", "second"]

    it "multiple blank lines collapsed" $
      byParagraph "first\n\n\n\nsecond" `shouldBe` ["first", "second"]

    it "leading and trailing blank lines ignored" $
      byParagraph "\n\nfirst\n\nsecond\n\n" `shouldBe` ["first", "second"]

    it "empty string gives no atoms" $
      byParagraph "" `shouldBe` []

    it "only whitespace gives no atoms" $
      byParagraph "\n\n\n" `shouldBe` []

    it "preserves internal newlines within a paragraph" $
      byParagraph "line one\nline two\n\nnext para"
        `shouldBe` ["line one\nline two", "next para"]

    it "three paragraphs" $
      byParagraph "a\n\nb\n\nc" `shouldBe` ["a", "b", "c"]

  describe "byParagraph / QuickCheck" $ do
    it "concatenation roundtrip: joining atoms with blank lines gives back something that re-splits the same way" $
      property $ \(NonEmpty chunks) ->
        let texts   = map (T.pack . getNonEmpty) chunks
            joined  = T.intercalate "\n\n" texts
            resplit = byParagraph joined
        in resplit == texts

    it "number of atoms is always >= 1 for non-empty input" $
      property $ \(NonEmpty s) ->
        let t = T.pack s
        in not (T.null (T.strip t)) ==>
           not (null (byParagraph t))
