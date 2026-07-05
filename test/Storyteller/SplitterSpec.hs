{-# LANGUAGE OverloadedStrings #-}

module Storyteller.SplitterSpec (spec) where

import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Storyteller.Common.Splitter (byParagraph, splitMarkdown)

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

  describe "splitMarkdown True (paragraph + heading aware)" $ do
    it "matches byParagraph when there are no headings" $
      splitMarkdown True "first\n\nsecond" `shouldBe` byParagraph "first\n\nsecond"

    it "splits at a heading with no surrounding blank lines" $
      splitMarkdown True "intro\n# Heading\nbody"
        `shouldBe` ["intro\n", "# Heading\nbody"]

    it "heading followed by a blank line still separates from its body" $
      splitMarkdown True "# Heading\n\nbody"
        `shouldBe` ["# Heading\n\n", "body"]

    it "multiple heading levels each start a new section" $
      splitMarkdown True "# H1\nbody1\n## H2\nbody2"
        `shouldBe` ["# H1\nbody1\n", "## H2\nbody2"]

    it "does not split on a '#' that isn't at the start of a line" $
      splitMarkdown True "a #b c" `shouldBe` ["a #b c"]

    it "still splits paragraphs within a heading's own section" $
      splitMarkdown True "# H1\npara1\n\npara2"
        `shouldBe` ["# H1\npara1\n\n", "para2"]

  describe "splitMarkdown False (heading only)" $ do
    it "keeps the whole text as one atom when there are no headings" $
      splitMarkdown False "para1\n\npara2" `shouldBe` ["para1\n\npara2"]

    it "splits only at headings, keeping paragraphs within a section together" $
      splitMarkdown False "# H1\npara1\n\npara2\n# H2\npara3"
        `shouldBe` ["# H1\npara1\n\npara2\n", "# H2\npara3"]

    it "empty string gives no atoms" $
      splitMarkdown False "" `shouldBe` []

  describe "splitMarkdown / QuickCheck" $ do
    it "is lossless for atParagraph = True" $
      property $ \s ->
        let t = T.pack s
        in T.concat (splitMarkdown True t) === t

    it "is lossless for atParagraph = False" $
      property $ \s ->
        let t = T.pack s
        in T.concat (splitMarkdown False t) === t
