{-# LANGUAGE OverloadedStrings #-}

-- | 'Storyteller.Writer.Branches' is the single place a branch *name* gets
-- classified by convention (see WRITER.md's "Branch naming"). Pins:
--
--   * @character/{id}@ is 'Character';
--   * the exact @"prompts"@ branch is 'Prompts';
--   * everything else, including a plain unprefixed name and the optional
--     @story/{storythread}@ prefix, is 'Story' — the default, not a
--     fallback for "unrecognized";
--   * 'branchDisplayName' strips a known prefix when present, otherwise
--     returns the name verbatim.
module Storyteller.BranchesSpec (spec) where

import Test.Hspec

import Storyteller.Writer.Branches

spec :: Spec
spec = do
  describe "classifyBranch" $ do
    it "recognizes a character branch" $
      classifyBranch "character/alice" `shouldBe` Character

    it "recognizes the well-known prompts branch" $
      classifyBranch "prompts" `shouldBe` Prompts

    it "treats a plain, unprefixed name as a story branch by default" $
      classifyBranch "master" `shouldBe` Story

    it "treats the optional story/ prefix as a story branch too" $
      classifyBranch "story/my-novel" `shouldBe` Story

    it "does not mistake a prompts-prefixed name for the exact prompts branch" $
      classifyBranch "prompts/extra" `shouldBe` Story

  describe "branchDisplayName" $ do
    it "strips the character/ prefix" $
      branchDisplayName "character/alice" `shouldBe` "alice"

    it "strips the optional story/ prefix" $
      branchDisplayName "story/my-novel" `shouldBe` "my-novel"

    it "leaves an unprefixed name untouched" $
      branchDisplayName "master" `shouldBe` "master"

    it "leaves the prompts branch untouched" $
      branchDisplayName "prompts" `shouldBe` "prompts"
