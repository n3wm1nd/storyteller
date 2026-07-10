{-# LANGUAGE OverloadedStrings #-}

module Storyteller.OutlineSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Storyteller.Writer.Agent.Outline (splitOnRule)

spec :: Spec
spec = describe "splitOnRule" $ do
  it "splits on a bare --- line, one group per side" $
    splitOnRule "chapter one\n---\nchapter two"
      `shouldBe` ["chapter one", "chapter two"]

  it "keeps each group's own lines in original order, not reversed" $
    splitOnRule "line a\nline b\nline c\n---\nline d\nline e"
      `shouldBe` ["line a\nline b\nline c", "line d\nline e"]

  it "handles more than two chapters" $
    splitOnRule "one\n---\ntwo\n---\nthree"
      `shouldBe` ["one", "two", "three"]

  it "trims surrounding blank lines from each group" $
    splitOnRule "\n\nchapter one\n\n---\n\nchapter two\n\n"
      `shouldBe` ["chapter one", "chapter two"]

  it "returns the whole text as one group when there's no delimiter" $
    splitOnRule "just one chapter, no rule anywhere"
      `shouldBe` ["just one chapter, no rule anywhere"]

  it "doesn't treat a longer or shorter rule as the delimiter" $
    splitOnRule "chapter one\n----\nstill chapter one\n--\nstill chapter one"
      `shouldBe` [T.strip "chapter one\n----\nstill chapter one\n--\nstill chapter one"]

  it "ignores a --- that isn't alone on its line" $
    splitOnRule "chapter one --- with a dash in prose\n---\nchapter two"
      `shouldBe` ["chapter one --- with a dash in prose", "chapter two"]
