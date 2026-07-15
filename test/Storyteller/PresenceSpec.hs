{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Storyteller.PresenceSpec (spec) where

import Data.List (find, sort)
import Data.Text (Text)
import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState)

import Git.Mock
import Storyteller.Core.Git hiding (emptyWorkingTree)
import Storyteller.Core.Storage (createBranch)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Core.Types
import Storyteller.Writer.Types (Character(..), Presence(..), PresenceEvent(..))
import Storyteller.Writer.Presence (recordPresence, activeCharactersFor, presentOn, presentAt)

-- | Every tick reachable from head, typed, oldest first -- the
-- "Storage.Tick"-based counterpart of @followChain@\/@fileTicksOf@ used
-- throughout this spec.
allTicks :: Members '[BranchOp Story] r => Sem r [Tick]
allTicks = runStorage @Story (do
  hashes <- Core.follow [] (\acc h _t -> (h : acc, True))
  mapM Tick.readTypesTick hashes)

-- ---------------------------------------------------------------------------
-- Phantom + runner
-- ---------------------------------------------------------------------------

data Story

-- | The character branches these tests exercise -- 'Character' (see
--   'Storyteller.Writer.Types') is the wrapped identity every presence
--   function actually takes; the raw 'BranchName' is only ever needed for
--   'createBranch'\/branch-opening calls, which want a real branch name,
--   not "a character" specifically.
alice, bob :: Character
alice = Character (BranchName "character/alice")
bob   = Character (BranchName "character/bob")

-- | Create "story" and, unless 'False' is passed, a "character/alice"
--   branch too, then run the action with 'Story''s scope already open —
--   'recordPresence' only ever touches the currently-open branch directly,
--   the character branch is referenced by name only (see the module comment
--   on 'Storyteller.Writer.Types.Presence').
runStory withCharacter action =
  run
  . runFail
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "story")
      if withCharacter then void (createBranch (unCharacter alice)) else return ()
      runBranchAndFS @Story (BranchName "story") action
  where void m = m >> return ()

-- | Commit @content@ as a real atom on @path@ -- the one thing that marks
--   "an atom happened" for 'trailingPresenceFor's purposes. Goes through
--   'Storage.Ops.addAtom' specifically (the real 'Storage.Core.Atom'
--   constructor, which is the one shape 'Storage.Core.store' actually
--   splices into the tree) rather than 'Tick.storeAs', which always wraps
--   its argument as a 'Storage.Core.NonAtom' -- content that merely
--   *decodes back* as atom-shaped on read, without ever landing in the
--   tree at all. That distinction didn't matter for this file's own
--   assertions (which only ever decode ticks back, never check tree
--   presence) until 'Storage.Tick.fileTicksOf' started checking real tree
--   presence for its own lifetime-boundary walk -- at which point a
--   "file" created this way was, correctly, still nonexistent.
writeAtom :: Members '[BranchOp Story, Fail] r => FilePath -> Text -> Sem r TickId
writeAtom path content = do
  h <- runStorage @Story (Ops.addAtom path content)
  return (TickId (Core.unObjectHash h))

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "recordPresence" $ do

    it "fails when the character branch does not exist" $
      runStory False (recordPresence @Story "scene.md" alice Enter)
        `shouldSatisfy` \case { Left _ -> True; Right _ -> False }

    it "produces a presence tick, visible in the chain, decodable back to the original event" $ do
      let result = runStory True $ do
            mtid  <- recordPresence @Story "scene.md" alice Enter
            ticks <- allTicks
            return (mtid >>= \tid -> find ((== tid) . tickId) ticks >>= fromTick @Presence)
      result `shouldBe` Right (Just (Presence "scene.md" alice Enter))

    it "records which file the presence event belongs to" $ do
      let result = runStory True $ do
            mtid  <- recordPresence @Story "chapters/ch1.md" alice Enter
            ticks <- allTicks
            return (mtid >>= \tid -> presenceFile <$> (find ((== tid) . tickId) ticks >>= fromTick @Presence))
      result `shouldBe` Right (Just "chapters/ch1.md")

    it "enter and leave round-trip independently when an atom separates them" $ do
      let result = runStory True $ do
            _     <- recordPresence @Story "scene.md" alice Enter
            _     <- writeAtom "scene.md" "she arrived.\n"
            mtl   <- recordPresence @Story "scene.md" alice Leave
            ticks <- allTicks
            return (mtl >>= \tid -> find ((== tid) . tickId) ticks >>= fromTick @Presence)
      result `shouldBe` Right (Just (Presence "scene.md" alice Leave))

    it "collapses a chain-adjacent redundant Enter into a single surviving Enter tick" $ do
      -- The first Enter is still "trailing" (nothing since it changed the
      -- file), so it gets squashed and replaced by the second — net one
      -- presence tick, not two, and not zero (the character is genuinely
      -- meant to end up active).
      let result = runStory True $ do
            -- A character can only enter a scene that exists.
            _           <- writeAtom "scene.md" ""
            ticksBefore <- runStorage @Story (Tick.fileTicksOf "scene.md")
            _           <- recordPresence @Story "scene.md" alice Enter
            mtid2       <- recordPresence @Story "scene.md" alice Enter
            ticksAfter  <- runStorage @Story (Tick.fileTicksOf "scene.md")
            return (mtid2, length ticksAfter - length ticksBefore)
      case result of
        Right (Just _, 1) -> return ()
        other -> expectationFailure ("expected a fresh single surviving Enter tick, got: " <> show other)

    it "rejects a Leave for a character that was never active" $ do
      let result = runStory True $
            recordPresence @Story "scene.md" alice Leave
      result `shouldBe` Right Nothing

    it "squashes an Enter immediately followed by a Leave with no atom in between" $ do
      let result = runStory True $ do
            _      <- recordPresence @Story "scene.md" alice Enter
            mtl    <- recordPresence @Story "scene.md" alice Leave
            ticks  <- runStorage @Story (Tick.fileTicksOf "scene.md")
            return (mtl, ticks)
      case result of
        Right (Nothing, ticks) -> ticks `shouldBe` []
        other -> expectationFailure ("expected fully squashed, empty chain, got: " <> show other)

    it "squashes a Leave immediately following an Enter, then treats a further re-Enter as fresh" $ do
      -- Leave squashes fully against the immediately-preceding Enter (net:
      -- as if neither had happened — see the "Enter immediately followed by
      -- Leave" case above). The Enter that follows has nothing left to
      -- squash against (the original tick was deleted), so it's a
      -- genuinely new 'recordPresence' call — its content (same file,
      -- character, event, and parent) may happen to hash identically under
      -- content-addressed storage, which is fine; what matters is exactly
      -- one presence tick survives with the right fields.
      let result = runStory True $ do
            _      <- writeAtom "scene.md" "opening.\n"
            _      <- recordPresence @Story "scene.md" alice Enter
            mtl    <- recordPresence @Story "scene.md" alice Leave
            mtid2  <- recordPresence @Story "scene.md" alice Enter
            ticks  <- runStorage @Story (Tick.fileTicksOf "scene.md")
            return (mtl, mtid2, [ ft | ft <- ticks, Tick.ftKind ft == "presence" ])
      case result of
        Right (Nothing, Just tid2, [presenceTick]) -> do
          TickId (Tick.ftTickId presenceTick) `shouldBe` tid2
          lookup "event" (Tick.ftFields presenceTick) `shouldBe` Just "enter"
        other -> expectationFailure ("expected a single fresh Enter tick, got: " <> show other)

    it "keeps a redundant Enter after an atom has intervened as still redundant (not chain-adjacent)" $ do
      let result = runStory True $ do
            _     <- recordPresence @Story "scene.md" alice Enter
            _     <- writeAtom "scene.md" "she stayed a while.\n"
            mtid2 <- recordPresence @Story "scene.md" alice Enter
            return mtid2
      result `shouldBe` Right Nothing

  describe "activeCharactersFor" $ do

    it "is empty on a file nobody has entered" $
      runStory True (activeCharactersFor @Story "scene.md")
        `shouldBe` Right []

    it "includes a character after Enter, mirroring the frontend's activeCharacterBranches fold" $
      runStory True (do
        -- A character can only enter a scene that exists.
        _ <- writeAtom "scene.md" ""
        _ <- recordPresence @Story "scene.md" alice Enter
        activeCharactersFor @Story "scene.md")
        `shouldBe` Right [alice]

    it "drops a character after Leave" $
      runStory True (do
        _ <- recordPresence @Story "scene.md" alice Enter
        _ <- writeAtom "scene.md" "she arrived.\n"
        _ <- recordPresence @Story "scene.md" alice Leave
        activeCharactersFor @Story "scene.md")
        `shouldBe` Right []

    it "is scoped per file -- a fresh file starts with nobody in it" $
      runStory True (do
        _ <- recordPresence @Story "scene.md" alice Enter
        activeCharactersFor @Story "other-scene.md")
        `shouldBe` Right []

    it "tracks multiple characters independently" $ do
      let result = runStory True $ do
            _ <- createBranch (unCharacter bob)
            -- A character can only enter a scene that exists.
            _ <- writeAtom "scene.md" ""
            _ <- recordPresence @Story "scene.md" alice Enter
            _ <- recordPresence @Story "scene.md" bob Enter
            active <- activeCharactersFor @Story "scene.md"
            return (sort active)
      result `shouldBe` Right (sort [alice, bob])

    it "resolves interleaved characters correctly: Alice squashes away, Bob's tick survives" $ do
      let result = runStory True $ do
            _     <- createBranch (unCharacter bob)
            -- A character can only enter a scene that exists.
            _     <- writeAtom "scene.md" ""
            _     <- recordPresence @Story "scene.md" alice Enter
            mtb   <- recordPresence @Story "scene.md" bob   Enter
            mtl   <- recordPresence @Story "scene.md" alice Leave
            ticks <- runStorage @Story (Tick.fileTicksOf "scene.md")
            let presenceTicks = [ ft | ft <- ticks, Tick.ftKind ft == "presence" ]
            return (mtb, mtl, map (lookup "character" . Tick.ftFields) presenceTicks)
      case result of
        Right (Just _tb, Nothing, [Just "character/bob"]) -> return ()
        other -> expectationFailure ("expected only Bob's (rebased) tick to survive, got: " <> show other)

  describe "presentOn" $ do

    it "is False on a file nobody has entered" $
      runStory True (runStorage @Story (presentOn "scene.md" alice))
        `shouldBe` Right False

    it "is True after Enter" $
      runStory True (do
        _ <- recordPresence @Story "scene.md" alice Enter
        runStorage @Story (presentOn "scene.md" alice))
        `shouldBe` Right True

    it "does not leak across files -- entering in one file leaves another untouched" $
      runStory True (do
        _ <- recordPresence @Story "chapters/ch1.md" alice Enter
        runStorage @Story (presentOn "chapters/ch2.md" alice))
        `shouldBe` Right False

  describe "presentAt" $ do

    it "answers differently for a tick before an Enter than for one after, within the same tracking pass" $ do
      let result = runStory True $ do
            beforeTid <- writeAtom "scene.md" "absent."
            _         <- recordPresence @Story "scene.md" alice Enter
            afterTid  <- writeAtom "scene.md" "\n\npresent."
            runStorage @Story
              ( (,) <$> presentAt beforeTid "scene.md" alice
                    <*> presentAt afterTid  "scene.md" alice )
      result `shouldBe` Right (False, True)

    it "does not leak across files when checking a specific tick" $ do
      let result = runStory True $ do
            _   <- recordPresence @Story "chapters/ch1.md" alice Enter
            tid <- writeAtom "chapters/ch2.md" "elsewhere."
            runStorage @Story (presentAt tid "chapters/ch2.md" alice)
      result `shouldBe` Right False
