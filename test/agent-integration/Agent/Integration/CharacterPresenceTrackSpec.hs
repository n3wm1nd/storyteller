{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Does 'Storyteller.Writer.Agent.PresenceTrack.trackPresenceFor' actually
--   work against a real LLM: given a scene file with no presence ticks at
--   all (the bulk-ingestion case this agent exists for -- an existing work
--   written before presence tracking existed) and two character branches,
--   does the model correctly recognize who's on-page from the prose alone
--   and record it?
--
--   Two character branches, each with one distinctive, checkable fact on
--   their @sheet.md@ (same fixture shape as
--   'Agent.Integration.CharacterPresenceSpec', which tests the read side --
--   this is the write side: can the agent itself *produce* the presence
--   ticks that spec's own scenario has to seed by hand). One scene where
--   both characters are on-page throughout, one distractor character never
--   mentioned at all -- the negative case ("don't mark someone who never
--   appears") is as much the point as the positive one.
module Agent.Integration.CharacterPresenceTrackSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (Members, Sem, embed)
import Polysemy.Fail (Fail)

import Runix.Git (Git)
import Runix.Logging (info)
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storage.Tick (FileTick(..))
import Storyteller.Core.Git (runBranchAndFS, runStorage)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..), TickId(..))
import Storyteller.Writer.Agent.PresenceTrack (PresenceDecision(..), trackPresenceFor)
import Storyteller.Writer.Presence (presentAt)
import Storyteller.Writer.Types (Character(..), PresenceEvent(..))

import Agent.Integration.Harness (Runner, quietSetup, runExpect)

-- | Phantom tag for opening one character branch's filesystem at a time --
--   same role 'Agent.Integration.CharacterPresenceSpec.Char_' plays there.
data Char_

sceneFile :: FilePath
sceneFile = "chapters/ch1.md"

rennickBranch, oyelaranBranch, absentBranch :: BranchName
rennickBranch  = BranchName "character/rennick"
oyelaranBranch = BranchName "character/oyelaran"
absentBranch   = BranchName "character/mireille"

rennickSheet, oyelaranSheet, absentSheet :: T.Text
rennickSheet  = "# Rennick\n\nAlways wears a chipped brass ring on his left thumb, never removes it.\n"
oyelaranSheet = "# Oyelaran\n\nHas a long scar along her jaw from a childhood accident, which she covers with her collar.\n"
absentSheet   = "# Mireille\n\nA reclusive cartographer who has not left her tower in a decade.\n"

sceneText :: T.Text
sceneText = T.unlines
  [ "Rennick shouldered through the market crowd, one hand closed tight around the brass ring on his"
  , "thumb, the way he always did when he was nervous. He spotted Oyelaran first -- she was leaning"
  , "against the fountain, absently touching the scar along her jaw, watching the crowd the way she"
  , "always watched crowds, like she expected trouble."
  , ""
  , "\"You're late,\" she said, not looking at him."
  , ""
  , "\"I'm exactly on time,\" Rennick said, and dropped onto the fountain's edge beside her. They stayed"
  , "there a long while, talking low, until the market bell rang and they both got up and walked off"
  , "together toward the harbor road."
  ]

seedCharacter
  :: forall r
  .  Members '[Git, StoryStorage, Fail] r
  => BranchName -> T.Text -> Sem r ()
seedCharacter branch sheet = do
  _ <- createBranch branch
  runBranchAndFS @Char_ branch $ runStorage @Char_ (Ops.saveFile "sheet.md" sheet)

spec
  :: forall judgeModel
  .  Runner judgeModel -> Spec
spec runner = describe "retroactive presence tracking (real LLM, cached)" $
  it "marks both on-page characters present, and never marks the one who doesn't appear" $
    runExpect @judgeModel runner $ do
      quietSetup $ do
        seedCharacter rennickBranch rennickSheet
        seedCharacter oyelaranBranch oyelaranSheet
        seedCharacter absentBranch absentSheet
        _ <- runStorage @Main (Ops.addAtom sceneFile sceneText)
        pure ()

      -- 'knownCast' enters each character branch itself (via the 'Branches'
      -- door) and reads its sheet through the ordinary filesystem effects,
      -- so there's no scope to name here beyond the tag the door is wired
      -- at.
      decisions <- trackPresenceFor @Main sceneFile
      info $ "presence decisions: " <> T.pack (show decisions)

      -- The scene's own text has both characters explicitly leave at the
      -- end ("they both got up and walked off... toward the harbor road"),
      -- so the file's *final* state correctly has nobody present -- that's
      -- not a bug to check against. What actually matters is whether both
      -- were on record as present *during* the scene, at the one atom that
      -- makes up its whole text -- via 'presentAt', anchored at that atom's
      -- own tick id, the same "as of this point in history" read
      -- 'Server.Writer.Branch.onlyWhilePresent' relies on in production.
      [atomTick] <- runStorage @Main (map ftTickId . filter ((/= Nothing) . ftContent) <$> Tick.fileTicksOf sceneFile)
      duringScene <- runStorage @Main $ do
        rennickHere  <- presentAt (TickId atomTick) sceneFile (Character rennickBranch)
        oyelaranHere <- presentAt (TickId atomTick) sceneFile (Character oyelaranBranch)
        absentHere   <- presentAt (TickId atomTick) sceneFile (Character absentBranch)
        pure (rennickHere, oyelaranHere, absentHere)
      embed $ duringScene `shouldBe` (True, True, False)

      embed $ decisions `shouldSatisfy` any (\(PresenceDecision c e _) -> c == rennickBranch && e == Enter)
      embed $ decisions `shouldSatisfy` any (\(PresenceDecision c e _) -> c == oyelaranBranch && e == Enter)
      embed $ decisions `shouldSatisfy` all (\(PresenceDecision c _ _) -> c /= absentBranch)
