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
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, readFile)

import Storyteller.Core.Git (BranchTag, BranchOp, runBranchAndFS, runStorage, atGeneric)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..), TickId(..), fromTick, tickParent)
import Storyteller.Writer.Agent (Prompt(..))
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick

import Server.Core.File (deleteFileAtom)
import Server.Writer.File (editChatPrompt)
import Server.TestStack

import Prelude hiding (readFile)

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
            h <- runStorage @Main (Tick.storeAs (Prompt "chat/f.md" "old text"))
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
        Right typed -> fromTick @Prompt typed `shouldBe` Just (Prompt "chat/f.md" "new text")

    -- The "regenerate with a changed prompt" flow
    -- ('Server.Writer.File.chatConverseSwipe') always calls 'editChatPrompt'
    -- with whatever (possibly stale) prompt id the client last saw, same as
    -- 'chatConverseSwipe''s own Haddock on why it reads history *before*
    -- editing. A second edit reusing the *original* (now-superseded) id --
    -- e.g. the user regenerates twice before the client's first response
    -- (carrying the new tick id) comes back -- must still land on the
    -- prompt's *current* text, not silently overwrite based on stale data
    -- or get lost. 'editChatPrompt' itself resolves the id inside its own
    -- 'Core.at' call, so the edit's *position* is never in question -- this
    -- pins that the *file* field it reads back off the (still-unresolved)
    -- 'Tick.readTypesTick' call survives a stale id too, since nothing here
    -- ever changes a 'Prompt''s own path in place.
    it "a second edit reusing the original (now-stale) prompt id still lands correctly" $ do
      let result = withFile_ runner (BranchName "b") $ do
            h <- runStorage @Main (Tick.storeAs (Prompt "chat/f.md" "v0"))
            let tid = TickId (Core.unObjectHash h)
            editChatPrompt tid "v1"
            editChatPrompt tid "v2"  -- same, now-stale, original id
            runStorage @Main (Core.resolveId h >>= Tick.readTypesTick)
      case result of
        Left err -> expectationFailure err
        Right typed -> fromTick @Prompt typed `shouldBe` Just (Prompt "chat/f.md" "v2")

  describe "correcting an instruction group (delete group, regenerate at its captured parent)" $ do

    -- Pins the assumption 'Server.Writer.File.correctGroup' depends on:
    -- the pivot for the rebased regeneration has to be the group's own
    -- prompt tick's *parent*, captured before any deletes run -- not the
    -- prompt tick itself. 'deleteFileAtom' (like every plain delete) drops
    -- a tick rather than replacing it, so it never gains a remap entry;
    -- once the prompt tick is gone, nothing could resolve it as an
    -- 'atGeneric' target anymore. Deleting the whole group *before*
    -- regenerating (rather than letting 'atGeneric' pop it as part of its
    -- own tail) is what keeps the group out of the tail that gets
    -- replayed back on top -- a tick still present when the rebase starts
    -- winding back would simply reappear.
    it "excises the whole group and replays what came after it once fresh content lands at the captured parent" $ do
      let result = withFile_ runner (BranchName "b") $ do
            promptH <- runStorage @Main (Tick.storeAs (Prompt "f.md" "write something"))
            atomHA  <- runStorage @Main (Ops.append "f.md" "atom A")
            atomHB  <- runStorage @Main (Ops.append "f.md" "atom B")
            _       <- runStorage @Main (Ops.append "f.md" "later content")

            let promptTid = TickId (Core.unObjectHash promptH)
            typed <- runStorage @Main (Tick.readTypesTick promptH)
            case tickParent typed of
              Nothing -> fail "correctGroup: prompt tick has no parent"
              Just parentTid -> do
                mapM_ deleteFileAtom [promptTid, TickId (Core.unObjectHash atomHA), TickId (Core.unObjectHash atomHB)]
                _ <- atGeneric @Main parentTid (runStorage @Main (Ops.append "f.md" "regenerated content"))
                readFile @(BranchTag Main) "f.md"
      case result of
        Left err -> expectationFailure err
        Right content -> content `shouldBe` "regenerated content\nlater content\n"
