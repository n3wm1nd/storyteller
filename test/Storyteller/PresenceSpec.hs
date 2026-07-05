{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Storyteller.PresenceSpec (spec) where

import Prelude hiding (appendFile)

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState)

import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, appendFile)

import Git.Mock
import Storyteller.Core.Atom (Atom(..))
import Storyteller.Core.Git hiding (emptyWorkingTree)
import Storyteller.Core.Storage
import Storyteller.Core.Types
import Storyteller.Writer.Types (Presence(..), PresenceEvent(..))
import Storyteller.Writer.Presence (recordPresence)

-- ---------------------------------------------------------------------------
-- Phantom + runner
-- ---------------------------------------------------------------------------

data Story

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
      if withCharacter then void (createBranch (BranchName "character/alice")) else return ()
      runBranchAndFS @Story (BranchName "story") action
  where void m = m >> return ()

-- | Append @content@ to @path@ and store it as an atom tick — the one thing
--   that marks "an atom happened" for 'trailingPresenceFor's purposes.
writeAtom
  :: Members '[ StoryBranch Story, FileSystem (BranchTag Story)
              , FileSystemRead (BranchTag Story), FileSystemWrite (BranchTag Story)
              , Fail ] r
  => FilePath -> Text -> Sem r TickId
writeAtom path content = do
  appendFile @(BranchTag Story) path (TE.encodeUtf8 content)
  storeAs @Story (Atom path content)

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "recordPresence" $ do

    it "fails when the character branch does not exist" $
      runStory False (recordPresence @Story "scene.md" (BranchName "character/alice") Enter)
        `shouldSatisfy` \case { Left _ -> True; Right _ -> False }

    it "produces a presence tick, visible in the chain, decodable back to the original event" $ do
      let result = runStory True $ do
            mtid  <- recordPresence @Story "scene.md" (BranchName "character/alice") Enter
            ticks <- follow @Story [] $ \acc t -> (t : acc, tickParent t)
            return (mtid >>= \tid -> find ((== tid) . tickId) ticks >>= fromTick @Presence)
      result `shouldBe` Right (Just (Presence "scene.md" (BranchName "character/alice") Enter))

    it "records which file the presence event belongs to" $ do
      let result = runStory True $ do
            mtid  <- recordPresence @Story "chapters/ch1.md" (BranchName "character/alice") Enter
            ticks <- follow @Story [] $ \acc t -> (t : acc, tickParent t)
            return (mtid >>= \tid -> presenceFile <$> (find ((== tid) . tickId) ticks >>= fromTick @Presence))
      result `shouldBe` Right (Just "chapters/ch1.md")

    it "enter and leave round-trip independently when an atom separates them" $ do
      let result = runStory True $ do
            _     <- recordPresence @Story "scene.md" (BranchName "character/alice") Enter
            _     <- writeAtom "scene.md" "she arrived.\n"
            mtl   <- recordPresence @Story "scene.md" (BranchName "character/alice") Leave
            ticks <- follow @Story [] $ \acc t -> (t : acc, tickParent t)
            return (mtl >>= \tid -> find ((== tid) . tickId) ticks >>= fromTick @Presence)
      result `shouldBe` Right (Just (Presence "scene.md" (BranchName "character/alice") Leave))

    it "collapses a chain-adjacent redundant Enter into a single surviving Enter tick" $ do
      -- The first Enter is still "trailing" (nothing since it changed the
      -- file), so it gets squashed and replaced by the second — net one
      -- presence tick, not two, and not zero (the character is genuinely
      -- meant to end up active).
      let result = runStory True $ do
            ticksBefore <- fileTicks @Story "scene.md"
            _           <- recordPresence @Story "scene.md" (BranchName "character/alice") Enter
            mtid2       <- recordPresence @Story "scene.md" (BranchName "character/alice") Enter
            ticksAfter  <- fileTicks @Story "scene.md"
            return (mtid2, length ticksAfter - length ticksBefore)
      case result of
        Right (Just _, 1) -> return ()
        other -> expectationFailure ("expected a fresh single surviving Enter tick, got: " <> show other)

    it "rejects a Leave for a character that was never active" $ do
      let result = runStory True $
            recordPresence @Story "scene.md" (BranchName "character/alice") Leave
      result `shouldBe` Right Nothing

    it "squashes an Enter immediately followed by a Leave with no atom in between" $ do
      let result = runStory True $ do
            _      <- recordPresence @Story "scene.md" (BranchName "character/alice") Enter
            mtl    <- recordPresence @Story "scene.md" (BranchName "character/alice") Leave
            ticks  <- fileTicks @Story "scene.md"
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
            _      <- recordPresence @Story "scene.md" (BranchName "character/alice") Enter
            mtl    <- recordPresence @Story "scene.md" (BranchName "character/alice") Leave
            mtid2  <- recordPresence @Story "scene.md" (BranchName "character/alice") Enter
            ticks  <- fileTicks @Story "scene.md"
            return (mtl, mtid2, [ ft | ft <- ticks, ftKind ft == "presence" ])
      case result of
        Right (Nothing, Just tid2, [presenceTick]) -> do
          TickId (ftTickId presenceTick) `shouldBe` tid2
          lookup "event" (ftFields presenceTick) `shouldBe` Just "enter"
        other -> expectationFailure ("expected a single fresh Enter tick, got: " <> show other)

    it "keeps a redundant Enter after an atom has intervened as still redundant (not chain-adjacent)" $ do
      let result = runStory True $ do
            _     <- recordPresence @Story "scene.md" (BranchName "character/alice") Enter
            _     <- writeAtom "scene.md" "she stayed a while.\n"
            mtid2 <- recordPresence @Story "scene.md" (BranchName "character/alice") Enter
            return mtid2
      result `shouldBe` Right Nothing

    it "resolves interleaved characters correctly: Alice squashes away, Bob's tick survives" $ do
      let result = runStory True $ do
            _     <- createBranch (BranchName "character/bob")
            _     <- recordPresence @Story "scene.md" (BranchName "character/alice") Enter
            mtb   <- recordPresence @Story "scene.md" (BranchName "character/bob")   Enter
            mtl   <- recordPresence @Story "scene.md" (BranchName "character/alice") Leave
            ticks <- fileTicks @Story "scene.md"
            let presenceTicks = [ ft | ft <- ticks, ftKind ft == "presence" ]
            return (mtb, mtl, map (lookup "character" . ftFields) presenceTicks)
      case result of
        Right (Just _tb, Nothing, [Just "character/bob"]) -> return ()
        other -> expectationFailure ("expected only Bob's (rebased) tick to survive, got: " <> show other)
