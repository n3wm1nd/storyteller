{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.CharacterSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (Sem, run)
import Runix.FileSystem (writeFile)

import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import Storyteller.Core.Git (BranchTag, BranchOp, runBranchAndFS)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))

import Server.Core.Branch (Main)
import Server.Writer.Character (CharacterState(..), characterState)
import Server.TestStack

import Prelude hiding (writeFile)

-- | 'characterState' is the plain 'Sem' function
--   'Server.Writer.Character.Connection' pushes over the wire — exercised
--   here directly, with no WebSocket/connection layer involved, same as
--   'Server.BranchSpec' tests 'Server.Core.Branch' directly. It's a pure
--   read with no writes of its own to test transactionally, so (unlike
--   'Server.BranchSpec'/'Server.FileSpec') this only runs under the eager
--   'testStack' — there's no buffered-vs-eager distinction for a function
--   that never calls 'StoryStorage' itself.
withCharacterBranch
  :: T.Text
  -> Sem ( FileSystemWrite (BranchTag Main)
         : FileSystemRead  (BranchTag Main)
         : FileSystem      (BranchTag Main)
         : BranchOp Main
         : StoryStorage
         : TestEffects '[] ) a
  -> Either String a
withCharacterBranch name action = run $ testStack $ do
  _ <- createBranch (BranchName name)
  runBranchAndFS @Main (BranchName name) action

spec :: Spec
spec = describe "characterState" $ do

  it "sheet is Nothing when sheet.md does not exist" $
    withCharacterBranch "character/nosheet" (characterState "character/nosheet")
      `shouldBe` Right (CharacterState "nosheet" Nothing)

  it "sheet is Just the file's content when sheet.md exists" $ do
    let result = withCharacterBranch "character/alice" $ do
          writeFile @(BranchTag Main) "sheet.md" "Alice is a curious explorer."
          characterState "character/alice"
    result `shouldBe` Right (CharacterState "alice" (Just "Alice is a curious explorer."))

  it "strips the character/ prefix from the display name" $
    withCharacterBranch "character/bob-the-builder" (characterState "character/bob-the-builder")
      `shouldSatisfy` either (const False) ((== "bob-the-builder") . charName)

  it "leaves the name untouched when there is no character/ prefix" $
    withCharacterBranch "not-a-character-branch" (characterState "not-a-character-branch")
      `shouldSatisfy` either (const False) ((== "not-a-character-branch") . charName)
