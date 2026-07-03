{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Storyteller.PresenceSpec (spec) where

import Data.List (find)
import Test.Hspec

import Polysemy
import Polysemy.Fail
import Polysemy.State (evalState)

import Git.Mock
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

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "recordPresence" $ do

    it "fails when the character branch does not exist" $
      runStory False (recordPresence @Story (BranchName "character/alice") Enter)
        `shouldSatisfy` \case { Left _ -> True; Right _ -> False }

    it "produces a presence tick, visible in the chain, decodable back to the original event" $ do
      let result = runStory True $ do
            tid   <- recordPresence @Story (BranchName "character/alice") Enter
            ticks <- follow @Story [] $ \acc t -> (t : acc, tickParent t)
            return (find ((== tid) . tickId) ticks >>= fromTick @Presence)
      result `shouldBe` Right (Just (Presence (BranchName "character/alice") Enter))

    it "enter and leave round-trip independently" $ do
      let result = runStory True $ do
            _  <- recordPresence @Story (BranchName "character/alice") Enter
            tl <- recordPresence @Story (BranchName "character/alice") Leave
            ticks <- follow @Story [] $ \acc t -> (t : acc, tickParent t)
            return (find ((== tl) . tickId) ticks >>= fromTick @Presence)
      result `shouldBe` Right (Just (Presence (BranchName "character/alice") Leave))
