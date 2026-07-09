{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Writer.FileSpec (spec) where

import Test.Hspec

import Polysemy (Sem, run)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)

import Storyteller.Core.Git (BranchTag, BranchOp, runBranchAndFS, runStorage)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..), TickId(..), fromTick)
import Storyteller.Writer.Agent (Prompt(..))
import qualified Storage.Core as Core
import qualified Storage.Tick as Tick

import Server.Writer.File (editChatPrompt)
import Server.TestStack

-- | 'editChatPrompt' only needs 'Server.Core.File.FileOpen', not the full
--   'SessionEffects' (no LLM call) — testable directly against
--   'Server.TestStack', same as 'Server.FileSpec'.
withFile_
  :: TestRunner
  -> BranchName
  -> Sem ( FileSystemWrite (BranchTag Main)
         : FileSystemRead  (BranchTag Main)
         : FileSystem      (BranchTag Main)
         : BranchOp Main
         : StoryStorage
         : TestEffects '[] ) a
  -> Either String a
withFile_ runner name action = run $ runner $ do
  _ <- createBranch name
  runBranchAndFS @Main name action

spec :: TestRunner -> Spec
spec runner = do

  describe "editChatPrompt" $ do

    -- Regression: a 'Prompt' always carries a "file" field (see its
    -- 'toDraft'), so its wire message is
    -- "file:<path>\n\ntype:prompt\n<text>" -- a naive edit that assumed
    -- the type tag sat on the very first line (true only for a tick with
    -- no fields at all) silently discarded the "type:prompt" tag along
    -- with the old text, corrupting the tick into something that decodes
    -- as an untyped, kindless message -- invisible to the chat UI ever
    -- after ('fromTick' would never again recognize it as a 'Prompt').
    -- This pins the fix: editing a prompt's text must leave it still
    -- decodable as a 'Prompt', with the same file and the new text.
    it "the edited tick still decodes as a Prompt, with its file preserved and text updated" $ do
      let result = withFile_ runner (BranchName "b") $ do
            (h, _) <- runStorage @Main (Tick.storeAs (Prompt "chat/f.md" "old text"))
            let tid = TickId (Core.unObjectHash h)
            editChatPrompt tid "new text"
            -- 'editChatPrompt' rebases the tick onto a new hash -- resolve
            -- @h@ (this scope's remap table persists across 'runStorage'
            -- dispatches, see 'Storyteller.Core.Git.runBranchOpGit') to
            -- read what it's actually become, rather than the stale
            -- pre-edit commit object still sitting under @h@ itself.
            runStorage @Main (Core.resolveId h >>= Tick.readTypesTick)
      case result of
        Left err -> expectationFailure err
        Right (typed, _) -> fromTick @Prompt typed `shouldBe` Just (Prompt "chat/f.md" "new text")
