{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | 'Storyteller.Writer.Agent.ContextFilter.hideBinaryFiles' -- the
--   interceptor that hides never-atom-tracked (binary) paths from an
--   agent's context assembly. The concrete bug this guards: a binary file
--   uploaded into a branch used to crash 'gatherFileContext'/'buildPreview'
--   outright (an unsafe UTF-8 decode of raw image bytes) rather than being
--   silently excluded.
module Storyteller.ContextFilterSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (Sem, run)

import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, listAllFiles, readFile, writeFile)

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Core.Git (BranchTag, BranchOp, runBranchAndFS, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))

import Server.Core.Branch (Main)
import Server.TestStack
import Storyteller.Writer.Agent.ContextFilter (PickerRule(..), applyContextLayout, hideBinaryFiles, hideChapters, hideLore)

import Prelude hiding (readFile, writeFile)

withFilterBranch
  :: T.Text
  -> Sem ( FileSystemWrite (BranchTag Main)
         : FileSystemRead  (BranchTag Main)
         : FileSystem      (BranchTag Main)
         : BranchOp Main
         : StoryStorage
         : TestEffects '[] ) a
  -> Either String a
withFilterBranch name action = run $ testStack $ do
  _ <- createBranch (BranchName name)
  runBranchAndFS @Main (BranchName name) action

spec :: Spec
spec = do
  hideBinaryFilesSpec
  hideChaptersSpec
  hideLoreSpec
  applyContextLayoutSpec

-- | 'hideChapters'/'hideLore' both build their 'PathFilter' from a
--   predicate over individual candidate *files* (e.g. "is this path a
--   chapter" or "is this path lore"), but 'listAllFiles' walks the tree
--   directory by directory ('Runix.FileSystem.foldTree'), and
--   'filterFileSystem' asks the very same predicate about each *directory*
--   it's about to descend into, not just the files it returns -- a
--   directory node that isn't itself a recognizable chapter/lore leaf
--   still has to read as "includable" or the whole listing underneath it
--   fails outright ('Runix.FileSystem.listFiles' hard-'fail's on any
--   denied 'ListFiles', it doesn't just quietly return no entries), rather
--   than the individual files under it just being narrowed out one by one
--   the way a reader of these two functions' Haddocks would expect.
hideChaptersSpec :: Spec
hideChaptersSpec = describe "hideChapters" $ do

  it "still lists a lore file that sits alongside a chapters/ directory (listing the directory itself must not be denied)" $ do
    let result = withFilterBranch "story" $ do
          _ <- runStorage @Main (Ops.addAtom "chapters/ch1.md" "p1\n")
          _ <- runStorage @Main (Ops.addAtom "notes/tavern.md" "not a chapter\n")
          hideChapters @(BranchTag Main) (listAllFiles @(BranchTag Main) "/")
    case result of
      Left err    -> expectationFailure err
      Right paths -> paths `shouldBe` ["notes/tavern.md"]

hideLoreSpec :: Spec
hideLoreSpec = describe "hideLore" $ do

  it "still lists a chapter that sits inside a chapters/ directory (listing the directory itself must not be denied, even though the directory's own name and every plain file beneath it independently satisfy the lore predicate)" $ do
    let result = withFilterBranch "story" $ do
          _ <- runStorage @Main (Ops.addAtom "chapters/ch1.md" "p1\n")
          _ <- runStorage @Main (Ops.addAtom "chapters/ch1.outline.md" "beat one\n")
          hideLore @(BranchTag Main) (listAllFiles @(BranchTag Main) "/")
    case result of
      Left err    -> expectationFailure err
      Right paths -> paths `shouldBe` ["chapters/ch1.md"]

  it "still lists a note that sits at the branch root (listing \"/\" itself must not be denied, even though root vacuously satisfies the lore predicate too)" $ do
    let result = withFilterBranch "story" $ do
          _ <- runStorage @Main (Ops.addAtom "notes/tavern.md" "a tavern\n")
          hideLore @(BranchTag Main) (listAllFiles @(BranchTag Main) "/")
    case result of
      Left err    -> expectationFailure err
      Right paths -> paths `shouldBe` []

hideBinaryFilesSpec :: Spec
hideBinaryFilesSpec = describe "hideBinaryFiles" $ do

  it "excludes a never-atom-tracked binary path from listAllFiles" $ do
    let result = withFilterBranch "story" $ do
          _ <- runStorage @Main (Ops.addAtom "scene.md" "p1\n")
          writeFile @(BranchTag Main) "portrait.png" "\xFF\xFE\x00"
          _ <- runStorage @Main (Ops.commitFiles ["portrait.png"])
          hideBinaryFiles @(BranchTag Main) @Main (listAllFiles @(BranchTag Main) "/")
    case result of
      Left err    -> expectationFailure err
      Right paths -> paths `shouldBe` ["scene.md"]

  it "leaves listAllFiles untouched when nothing is binary" $ do
    let result = withFilterBranch "story" $ do
          _ <- runStorage @Main (Ops.addAtom "scene.md" "p1\n")
          _ <- runStorage @Main (Ops.addAtom "other.md" "p2\n")
          hideBinaryFiles @(BranchTag Main) @Main (listAllFiles @(BranchTag Main) "/")
    case result of
      Left err    -> expectationFailure err
      Right paths -> paths `shouldMatchList` ["scene.md", "other.md"]

  it "fails a readFile attempted on a hidden binary path, instead of crashing on the bytes themselves" $ do
    let result = withFilterBranch "story" $ do
          writeFile @(BranchTag Main) "portrait.png" "\xFF\xFE\x00"
          _ <- runStorage @Main (Ops.commitFiles ["portrait.png"])
          hideBinaryFiles @(BranchTag Main) @Main (readFile @(BranchTag Main) "portrait.png")
    result `shouldBe` Left "Access denied: binary files are hidden"

-- | 'applyContextLayout' -- the bucket-picker primitive behind the
--   ContextLayout a client can attach to a 'chat.writer' command (see
--   'Storyteller.Writer.Agent.Continuation.gatherFileContext'). Pure, so no
--   branch/effect scaffolding needed.
applyContextLayoutSpec :: Spec
applyContextLayoutSpec = describe "applyContextLayout" $ do

  it "orders paths by ascending bucket, filename-sorted within a bucket" $
    applyContextLayout
      [ PickerRule "chapters/*" (Just 3), PickerRule "**/*" (Just 1) ]
      ["chapters/ch2.md", "outline.md", "chapters/ch1.md", "notes.md"]
    `shouldBe` ["notes.md", "outline.md", "chapters/ch1.md", "chapters/ch2.md"]

  it "claim order (rule position) is independent of bucket order: a narrow rule claims ahead of a broad catch-all regardless of bucket number" $
    applyContextLayout
      [ PickerRule "outline.md" (Just 1), PickerRule "**/*outline.md" Nothing, PickerRule "**/*" (Just 2) ]
      ["outline.md", "chapters/ch1.outline.md", "chapters/ch1.md"]
    `shouldBe` ["outline.md", "chapters/ch1.md"]

  it "drops a path no rule claims (implicit trash), same as an explicit trash rule" $
    applyContextLayout
      [ PickerRule "keep.md" (Just 1) ]
      ["keep.md", "ignored.md"]
    `shouldBe` ["keep.md"]

  it "an explicit trash rule wins even when a later rule would also match" $
    applyContextLayout
      [ PickerRule "secret.md" Nothing, PickerRule "**/*" (Just 1) ]
      ["secret.md", "public.md"]
    `shouldBe` ["public.md"]

  it "an empty layout claims nothing (callers decide what '[]' means to them)" $
    applyContextLayout [] ["a.md", "b.md"] `shouldBe` []
