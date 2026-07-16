{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Writer.BranchSpec (spec) where

import qualified Data.ByteString as BS
import Data.Maybe (mapMaybe)
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Polysemy (Sem, run)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, listFiles, listAllFiles, readFile)
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Common.Types (Note(..))
import Storyteller.Core.Git (BranchTag, BranchOp, runBranchAndFS, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..), fromTick)

import Server.Core.Branch (Main)
import Server.Writer.Branch (summarize, uploadFiles, importCharacterCard)
import Server.TestStack
import Storyteller.Core.Storage (getBranch)
import Storyteller.Writer.Agent.SummaryAccess (densest)

import Prelude hiding (readFile)

-- | 'uploadFiles' is the plain 'Sem' function 'Server.Writer.Branch.uploadFile'
--   calls for the HTTP PUT upload endpoint, exercised here directly with no
--   HTTP layer involved — same as 'Server.BranchSpec' tests
--   'Server.Core.Branch' directly. It does write through 'StoryStorage' (via
--   'commitFiles'), so — like the mutating operations in 'Server.BranchSpec'
--   — it's run under both 'runner' variants (see 'test/Main.hs'): eager, and
--   buffered through 'Storyteller.Core.Git.withStorage', the transaction a
--   real upload
--   command actually runs inside.
withBranch_
  :: TestRunner
  -> BranchName
  -> Sem ( FileSystemWrite (BranchTag Main)
         : FileSystemRead  (BranchTag Main)
         : FileSystem      (BranchTag Main)
         : BranchOp Main
         : StoryStorage
         : TestEffects '[] ) a
  -> Either String a
withBranch_ runner name action = run $ runner $ do
  _ <- createBranch name
  runBranchAndFS @Main name action

spec :: TestRunner -> Spec
spec runner = do
  uploadFilesSpec runner
  summarizeSpec runner
  importCharacterCardSpec runner

uploadFilesSpec :: TestRunner -> Spec
uploadFilesSpec runner = describe "uploadFiles" $ do

  it "creates a new file with the uploaded content" $ do
    let result = withBranch_ runner (BranchName "test") $ do
          _ <- uploadFiles [("dropped.md", "hello world")]
          content <- readFile @(BranchTag Main) "dropped.md"
          return (TE.decodeUtf8 content)
    result `shouldBe` Right "hello world"

  it "the uploaded path appears in the branch's file list" $ do
    let result = withBranch_ runner (BranchName "test") $ do
          _ <- uploadFiles [("dropped.md", "hello world")]
          listFiles @(BranchTag Main) "/"
    result `shouldSatisfy` either (const False) (elem "dropped.md")

  it "uploads multiple files in one call" $ do
    let result = withBranch_ runner (BranchName "test") $ do
          paths <- uploadFiles [("a.md", "content a"), ("folder/b.md", "content b")]
          files <- listAllFiles @(BranchTag Main) "/"
          return (paths, files)
    case result of
      Left err -> expectationFailure err
      Right (paths, files) -> do
        paths `shouldBe` ["a.md", "folder/b.md"]
        files `shouldContain` ["a.md"]
        files `shouldContain` ["folder/b.md"]

  it "returns the uploaded paths, so the caller can push FileAdded events" $ do
    let result = withBranch_ runner (BranchName "test") $
          uploadFiles [("one.md", "1"), ("two.md", "2")]
    result `shouldBe` Right ["one.md", "two.md"]

  it "an uploaded file's content survives being read back after a second, unrelated upload" $ do
    -- Regression check for 'commitFiles' being scoped to just the given
    -- paths (see Storyteller.Core.Edit) rather than reconciling the whole
    -- branch: a second, unrelated upload must not disturb an already
    -- uploaded file's content.
    let result = withBranch_ runner (BranchName "test") $ do
          _ <- uploadFiles [("first.md", "first content")]
          _ <- uploadFiles [("second.md", "second content")]
          readFile @(BranchTag Main) "first.md"
    fmap TE.decodeUtf8 result `shouldBe` Right "first content"

  -- 'uploadFiles' always deposits via 'Ops.addBinary' now (see its own
  -- Haddock) — an upload is a deposit, not a claim the bytes are prose,
  -- so it never becomes atom-tracked on its own, whether or not the
  -- content happens to decode as UTF-8. Promoting a path to atom-tracked
  -- text is a separate, deliberate "ingest" action (not yet built).
  it "a non-UTF8 upload is not atom-tracked, and its exact bytes survive" $ do
    let bytes = BS.pack [0xFF, 0xFE, 0x00, 0x01]
    let result = withBranch_ runner (BranchName "test") $ do
          _       <- uploadFiles [("portrait.png", bytes)]
          content <- readFile @(BranchTag Main) "portrait.png"
          tracked <- runStorage @Main (Ops.hasAnyAtom "portrait.png")
          return (content, tracked)
    result `shouldBe` Right (bytes, False)

  it "a plain-text upload is also not atom-tracked -- it stays an opaque asset until explicitly ingested" $ do
    let result = withBranch_ runner (BranchName "test") $ do
          _ <- uploadFiles [("notes.md", "hello")]
          tracked <- runStorage @Main (Ops.hasAnyAtom "notes.md")
          return tracked
    result `shouldBe` Right False

-- | 'summarize' is the WS 'Server.Writer.Branch.Protocol.Summarize'
--   command's own implementation -- exercised directly here (no WS/JSON
--   layer), same as 'uploadFiles' above. Uses the generic placeholder
--   generator ('Server.Writer.Branch.passthroughGenerate', not exported) via
--   a kind other than @"prose/chapter"@ (which is LLM-backed now -- see
--   'Storyteller.Writer.Agent.ChapterSummarizer' -- and so isn't exercised
--   through this plain 'TestRunner' stack, same as no other agent's real
--   'queryLLM' call is unit-tested), so this checks the generic wiring -- a
--   real 'Storyteller.Common.Summary.Summary' tick gets produced and is
--   discoverable through 'Storyteller.Writer.Agent.SummaryAccess' -- not any
--   real compression.
summarizeSpec :: TestRunner -> Spec
summarizeSpec runner = describe "summarize" $ do
  it "produces a Summary tick whose content is discoverable via densest" $ do
    let result = withBranch_ runner (BranchName "test") $ do
          _    <- runStorage @Main (Ops.addAtom "story.md" "chapter one.")
          mtid <- summarize "raw"
          content <- densest @Main ["raw"] "story.md"
          return (mtid, content)
    case result of
      Left err -> expectationFailure err
      Right (mtid, content) -> do
        mtid `shouldNotBe` Nothing
        content `shouldBe` "chapter one."

  it "a second call with nothing new returns Nothing" $ do
    let result = withBranch_ runner (BranchName "test") $ do
          _ <- runStorage @Main (Ops.addAtom "story.md" "chapter one.")
          _ <- summarize "raw"
          summarize "raw"
    result `shouldBe` Right Nothing

-- | 'importCharacterCard' is the atomic branch-creation counterpart to
--   'uploadFiles' -- the SillyTavern character-card-import command's own
--   implementation (see 'Server.Writer.Session.Dispatch'). Unlike
--   'uploadFiles', which deposits into an already-open branch, this opens
--   its own scope after creating the branch, so it's exercised at the top
--   level of the runner rather than through 'withBranch_'.
importCharacterCardSpec :: TestRunner -> Spec
importCharacterCardSpec runner = describe "importCharacterCard" $ do

  it "creates the branch and writes every given file with its given content" $ do
    let result = run $ runner $ do
          importCharacterCard (BranchName "character/alice")
            [ ("sheet.md", "Alice is a curious explorer.")
            , ("instructions.md", "Speaks in short sentences.")
            ]
            Nothing Nothing
          runBranchAndFS @Main (BranchName "character/alice") $ do
            sheet        <- readFile @(BranchTag Main) "sheet.md"
            instructions <- readFile @(BranchTag Main) "instructions.md"
            return (TE.decodeUtf8 sheet, TE.decodeUtf8 instructions)
    result `shouldBe` Right ("Alice is a curious explorer.", "Speaks in short sentences.")

  it "the branch is discoverable via getBranch afterwards" $ do
    let result = run $ runner $ do
          importCharacterCard (BranchName "character/bob") [("sheet.md", "Bob.")] Nothing Nothing
          getBranch (BranchName "character/bob")
    result `shouldSatisfy` either (const False) (/= Nothing)

  -- Contrast with 'uploadFiles', which deliberately deposits as an opaque,
  -- never-atom-tracked asset (see its own Haddock) -- an imported card's
  -- files are real prose/content the user will go on to edit normally, so
  -- they need to reconcile against the atom chain like any other write,
  -- not sit as an opaque binary blob.
  it "deposited files are atom-tracked, unlike a plain upload" $ do
    let result = run $ runner $ do
          importCharacterCard (BranchName "character/carol") [("sheet.md", "Carol.")] Nothing Nothing
          runBranchAndFS @Main (BranchName "character/carol") $
            runStorage @Main (Ops.hasAnyAtom "sheet.md")
    result `shouldBe` Right True

  -- The card's provenance (imported-from/creator attribution, creator
  -- notes) is metadata about the import for the human author, not part of
  -- what an agent reading sheet.md should treat as the character's
  -- identity/voice -- see this function's own Haddock. It lands as a
  -- free-floating Note tick instead, found here the same way
  -- 'Storyteller.Common.Annotation.addNote' itself checks ref validity:
  -- walking every tick reachable from the branch head.
  it "deposits the note as a free-floating Note tick, not sheet.md prose" $ do
    let result = run $ runner $ do
          importCharacterCard (BranchName "character/erin") [("sheet.md", "Erin.")] Nothing
            (Just "Imported from a SillyTavern character card by Some Creator.")
          runBranchAndFS @Main (BranchName "character/erin") $ do
            sheet <- readFile @(BranchTag Main) "sheet.md"
            (_, ticks) <- runStorage @Main (Tick.newTypesTicksSince Nothing)
            return (TE.decodeUtf8 sheet, mapMaybe (fromTick @Note) ticks)
    case result of
      Left err -> expectationFailure err
      Right (sheet, notes) -> do
        sheet `shouldBe` "Erin."
        map noteBody notes `shouldBe` ["Imported from a SillyTavern character card by Some Creator."]
        map noteRefs notes `shouldBe` [[]]

  -- The avatar rides in the same atomic command now (see this function's
  -- own Haddock for why: a separate follow-up PUT raced the branch's own
  -- creation in practice), deposited the same opaque, never-atom-tracked
  -- way 'uploadFiles' deposits any binary asset.
  it "deposits the avatar alongside the text files when given one" $ do
    let bytes = BS.pack [0x89, 0x50, 0x4e, 0x47]
    let result = run $ runner $ do
          importCharacterCard (BranchName "character/dana") [("sheet.md", "Dana.")] (Just ("avatar.png", bytes)) Nothing
          runBranchAndFS @Main (BranchName "character/dana") $ do
            content <- readFile @(BranchTag Main) "avatar.png"
            tracked <- runStorage @Main (Ops.hasAnyAtom "avatar.png")
            return (content, tracked)
    result `shouldBe` Right (bytes, False)
