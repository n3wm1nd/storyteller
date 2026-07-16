{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Is a character actually *known* to the writer once it's added to a
--   scene -- through the real pipeline (a character branch, a 'Presence'
--   tick recording them entering a scene file, 'activeCharactersFor'
--   reading that back), not a hand-built 'CharContextBlock' list the way
--   'Agent.Integration.CharContextWriteSpec' already checks.
--
--   Two character branches, each with one distinctive, checkable fact on
--   their @sheet.md@. Both enter the same scene file via
--   'Storyteller.Writer.Presence.recordPresence'; the scene is then written
--   with 'writeAgent' fed exactly the 'CharSummary's
--   'Server.Writer.File.activeCharacterContext' would have built for them
--   (via 'charSummaryWithJournal', same call), not anything shortcut past
--   presence. A real LLM call, cached under
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
import Storyteller.Writer.Agent (CharLabel(..), CharSummary(..), Instruction(..), Prose(..))
import Storyteller.Writer.Agent.CharContext (charSummaryWithJournal)
import Storyteller.Writer.Agent.Write (writeAgent)
import Storyteller.Writer.Presence (activeCharactersFor, recordPresence)
import Storyteller.Writer.Types (Character(..), PresenceEvent(Enter))

import Agent.Integration.Harness (Runner, runExpect)
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

-- | Create a character branch and seed its @sheet.md@ -- the one-time setup
--   'charSummaryWithJournal' below reads back.
seedCharacter
  :: forall r
  .  Members '[Git, StoryStorage, Fail] r
  => BranchName -> T.Text -> Sem r ()
seedCharacter branch sheet = do
  _ <- createBranch branch
  runBranchAndFS @Char_ branch $ runStorage @Char_ (Ops.saveFile "sheet.md" sheet)

-- | Read one active character's context back exactly the way
--   'Server.Writer.File.activeCharacterContext' does -- @sheet.md@ plus
--   nothing else (no other context files planted here, no journal), so
--   'csJournal' comes back empty; this scenario is about sheet identity,
--   not journal (see 'Agent.Integration.JournalInstructionSpec'/
--   'JournalIronySpec' for that).
summarize
  :: forall r
  .  Members '[Git, StoryStorage, Fail] r
  => Character -> Sem r (CharLabel, CharSummary)
summarize (Character branch) = do
  summary <- runBranchAndFS @Char_ branch $
    runStorage @Char_ (charSummaryWithJournal "sheet.md" "journal.md" (const True) 30 10 2)
  let label = maybe (unBranchName branch) id (T.stripPrefix "character/" (unBranchName branch))
  pure (CharLabel label, summary)

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

      _ <- recordPresence @Main sceneFile (Character rennickBranch) Enter
      _ <- recordPresence @Main sceneFile (Character oyelaranBranch) Enter
      active <- activeCharactersFor @Main sceneFile
      info $ "active characters: " <> T.pack (show active)
      embed $ length active `shouldBe` 2

      chars <- mapM summarize active
      Prose text <- writeAgent [] [] chars [] [] [] instruction
      info ("writeAgent output:\n" <> text)
      embed $ text `shouldNotBe` ""
      judgeOrFail @judgeModel text judgeQuestion
