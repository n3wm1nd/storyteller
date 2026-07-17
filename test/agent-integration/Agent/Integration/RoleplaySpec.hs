{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Does the roleplay writer's two-tier design ('Storyteller.Writer.Agent.
--   Roleplay') actually hold up its central promise -- that each present
--   character's own journal reflects only what *they* could plausibly
--   perceive, not an omniscient narrator's account -- against a real LLM,
--   or is per-character knowledge separation just a plausible-sounding idea
--   that collapses the moment a model actually has to keep it straight?
--
--   Two characters share a scene, deliberately given asymmetric knowledge:
--   Nadia privately diverted partnership funds to cover a personal debt;
--   Owen has no idea, and is about to praise how transparent their
--   partnership has always been. 'runRoleplayTurn' runs the full pipeline:
--   interrogate both characters via 'Storyteller.Writer.Agent.Roleplay.
--   roleplayAgent' (a direct, unconditional ask of every present character,
--   not a tool loop), write the scene, then run 'Storyteller.Writer.Agent.
--   Roleplay.characterReflectAgent' for both. Both structural checks (did
--   each character's journal actually gain its own, distinct entry) and a
--   judge check (does Owen's own entry avoid the secret Nadia's carries)
--   run against the real result.
--
--   The simplest possible scenario this design has to hold up under -- see
--   'Agent.Integration.RoleplayMidStorySpec' for a fuller one (established
--   history, tasks.md, per-character notes about each other).
module Agent.Integration.RoleplaySpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed)
import Runix.Logging (info)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import qualified Storage.Ops as Ops
import Storyteller.Core.Git (runBranchAndFS, runStorage)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent (CharLabel(..), Prose(..))
import Storyteller.Writer.Presence (recordPresence)
import Storyteller.Writer.Types (Character(..), PresenceEvent(..))

import Agent.Integration.Harness (Runner, runExpect)
import Agent.Integration.Judge (judgeOrFail)
import Agent.Integration.RoleplayHarness (runRoleplayTurn)

-- | Phantom tag for opening either character branch this scenario uses, one
--   at a time.
data Char_

nadiaBranch, owenBranch :: BranchName
nadiaBranch = BranchName "character/nadia"
owenBranch  = BranchName "character/owen"

scenePath :: FilePath
scenePath = "chapters/scene1.md"

nadiaSheet :: T.Text
nadiaSheet = "# Nadia\n\nOwen's business partner -- careful, guarded, and very good at sounding calm.\n"

nadiaJournal :: T.Text
nadiaJournal = T.unwords
  [ "I moved part of the partnership's reserve to cover my own debt last"
  , "month. Owen has no idea -- as far as he knows the numbers are exactly"
  , "what they've always been."
  ]

owenSheet :: T.Text
owenSheet = "# Owen\n\nNadia's business partner -- trusting, and quietly proud that their partnership has never had any hidden surprises.\n"

direction :: T.Text
direction = T.unwords
  [ "Owen pulls up the quarterly numbers and tells Nadia how relieved he is"
  , "that, unlike every other partnership he's heard of, theirs has never"
  , "had a single hidden surprise."
  ]

judgeQuestion :: T.Text
judgeQuestion = T.unwords
  [ "Nadia secretly diverted partnership funds to cover a personal debt;"
  , "Owen has no idea and is praising how transparent their partnership has"
  , "always been. Below is Owen's own private journal entry for this scene."
  , "Does it stay consistent with someone who has NOT been told about the"
  , "diverted funds -- i.e. it never states, implies, or reacts as though"
  , "Owen knows about the missing money? Answer no if Owen's entry shows any"
  , "awareness of the secret."
  ]

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "the roleplay writer's per-character knowledge separation (real LLM, cached)" $
  it "writes a scene from both characters' own answers, then journals each character's own, distinct perception of it" $
    runExpect @judgeModel runner $ do
      _ <- createBranch nadiaBranch
      _ <- createBranch owenBranch

      runBranchAndFS @Char_ nadiaBranch $ runStorage @Char_ $ do
        Ops.saveFile "sheet.md" nadiaSheet
        _ <- Ops.append "journal.md" nadiaJournal
        pure ()
      runBranchAndFS @Char_ owenBranch $ runStorage @Char_ (Ops.saveFile "sheet.md" owenSheet)

      -- A presence tick only marks who's in a scene that already exists --
      -- scenePath has to actually land in the tree first (a real atom, not
      -- just a tick that mentions its path), or fileTicksOf's
      -- tree-presence-scoped walk correctly finds nothing to attach the
      -- presence ticks below to. See CharacterPresenceSpec's own note.
      _ <- runStorage @Main (Ops.addAtom scenePath "")

      _ <- recordPresence @Main scenePath (Character nadiaBranch) Enter
      _ <- recordPresence @Main scenePath (Character owenBranch) Enter

      (Prose narrative, entries) <- runRoleplayTurn scenePath direction
      info ("roleplay narrative:\n" <> narrative)
      embed $ narrative `shouldNotBe` ""

      let nadiaEntry = maybe "" id (lookup (CharLabel "nadia") entries)
          owenEntry  = maybe "" id (lookup (CharLabel "owen")  entries)
      info ("Nadia's journal entry: " <> nadiaEntry)
      info ("Owen's journal entry: " <> owenEntry)
      embed $ nadiaEntry `shouldNotBe` ""
      embed $ owenEntry `shouldNotBe` ""
      -- The bug this whole feature exists to avoid: the same narrator's-eye
      -- account getting copy-pasted into every character's journal instead
      -- of each writing their own. A structural, not qualitative, guard.
      embed $ nadiaEntry `shouldNotBe` owenEntry
      embed $ owenEntry `shouldNotBe` narrative
      embed $ nadiaEntry `shouldNotBe` narrative

      judgeOrFail @judgeModel owenEntry judgeQuestion
