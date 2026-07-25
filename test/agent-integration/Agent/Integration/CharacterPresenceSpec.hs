{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Is a character actually *known* to the writer once it's added to a
--   scene -- through the real pipeline (a character branch, a 'Presence'
--   tick recording them entering a scene file), with no help from this
--   scenario at all: 'writeAgent' now reads presence and every active
--   character's own context for itself (see its own Haddock) -- there's
--   no @chars@ parameter left for a scenario to build by hand, so this is
--   a direct test of that internal gathering, not a simulation of it the
--   way an earlier version of this file (manually calling
--   'activeCharactersFor'\/'resolveContext1' itself) had to be.
--
--   Two character branches, each with one distinctive, checkable fact on
--   their @sheet.md@. Both enter the same scene file via
--   'Storyteller.Writer.Presence.enters'; the scene is then
--   written with a plain 'writeAgent' call, nothing about characters
--   threaded through explicitly. A real LLM call, cached under
--   test/fixtures/llm-agent-cache/agent/.
module Agent.Integration.CharacterPresenceSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (Members, Sem, embed)
import Polysemy.Fail (Fail)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import Runix.Git (Git)
import Runix.Logging (info)
import qualified Storage.Ops as Ops
import Storyteller.Core.Git (runBranchAndFS, runStorage)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent (Instruction(..), Prose(..), PastChaptersMode(..))
import Storyteller.Writer.Agent.Write (writeAgent)
import Storyteller.Writer.Presence (activeCharactersFor, enters)
import Storyteller.Writer.Types (Character(..))

import Agent.Integration.Harness (Runner, emptyPinnedContext, emptyLore, runExpect)
import Agent.Integration.Judge (judgeOrFail)

-- | Phantom tag for opening one character branch's filesystem at a time --
--   same role 'Server.Writer.File.ActiveChar' plays in production, local
--   here since nothing outside this module needs to name it.
data Char_

sceneFile :: FilePath
sceneFile = "chapters/ch1.md"

rennickBranch, oyelaranBranch :: BranchName
rennickBranch  = BranchName "character/rennick"
oyelaranBranch = BranchName "character/oyelaran"

rennickSheet, oyelaranSheet :: T.Text
rennickSheet  = "# Rennick\n\nAlways wears a chipped brass ring on his left thumb, never removes it.\n"
oyelaranSheet = "# Oyelaran\n\nHas a long scar along her jaw from a childhood accident, which she covers with her collar.\n"

instruction :: Instruction
instruction = Instruction $ T.unwords
  [ "Write the moment Rennick and Oyelaran first spot each other across a"
  , "crowded market. Each should notice one specific, distinguishing detail"
  , "about the other that lets them recognize who they are."
  ]

judgeQuestion :: T.Text
judgeQuestion = T.unwords
  [ "Does this text have Oyelaran noticing Rennick's brass ring (not a scar),"
  , "and Rennick noticing Oyelaran's jaw scar (not a ring)? Answer no if the"
  , "details are swapped between the two characters, or if either detail is"
  , "missing entirely."
  ]

-- | Create a character branch and seed its @sheet.md@ -- what 'writeAgent'
--   itself reads back once presence marks the branch active on the scene.
seedCharacter
  :: forall r
  .  Members '[Git, StoryStorage, Fail] r
  => BranchName -> T.Text -> Sem r ()
seedCharacter branch sheet = do
  _ <- createBranch branch
  runBranchAndFS @Char_ branch $ runStorage @Char_ (Ops.saveFile "sheet.md" sheet)

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "characters present in a scene (real LLM, cached)" $
  it "reflects each active character's own distinguishing sheet detail, correctly attributed" $
    runExpect @judgeModel runner $ do
      seedCharacter rennickBranch rennickSheet
      seedCharacter oyelaranBranch oyelaranSheet

      -- A presence tick only marks who's in a scene that already exists --
      -- 'chapters/ch1.md' has to actually land in the tree first (a real
      -- atom, not just a tick that mentions its path), or fileTicksOf's
      -- tree-presence-scoped walk correctly finds nothing to attach the
      -- presence ticks below to. See PresenceSpec's own 'writeAtom' for the
      -- same requirement.
      _ <- runStorage @Main (Ops.addAtom sceneFile "")

      _ <- enters @Main sceneFile (Character rennickBranch)
      _ <- enters @Main sceneFile (Character oyelaranBranch)
      active <- activeCharactersFor @Main sceneFile
      info $ "active characters: " <> T.pack (show active)
      embed $ length active `shouldBe` 2

      Prose text <- writeAgent @Main sceneFile emptyLore FullChapters emptyPinnedContext instruction
      info ("writeAgent output:\n" <> text)
      embed $ text `shouldNotBe` ""
      judgeOrFail @judgeModel text judgeQuestion
