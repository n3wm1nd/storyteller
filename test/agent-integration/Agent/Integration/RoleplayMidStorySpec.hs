{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The same question 'Agent.Integration.RoleplaySpec' asks, but against a
--   scenario closer to what the roleplay writer would actually face
--   mid-story, not the simplest case that could possibly work: two
--   characters with an established shared history (real prior scene beats
--   already on the chain, not an empty file), each carrying the full file
--   set 'Storyteller.Writer.Agent.Roleplay.characterIntentAgent' is meant
--   to read and use -- @sheet.md@ (fixed self), @characters/*.md@ (their
--   own, possibly imperfect notes about the other -- never the other's
--   inner thoughts, since there's no channel for that), @tasks.md@ (what
--   they're actually trying to accomplish this chapter), and @journal.md@
--   (their own filtered history so far, not a copy of the shared record).
--
--   Ren and Iskra are running a long con on a mark, Elias Thorne, three
--   beats in. Both have independently, privately started to feel guilt
--   about it -- and *neither knows the other has*, since each only knows
--   what the other's own @characters/*.md@ entry says about them, written
--   before either started wavering. That's the dramatic-irony axis this
--   scenario is built to test, and it's a harder version of
--   'Agent.Integration.RoleplaySpec''s own one-directional secret: two
--   independent private facts, each hidden from the other character (not
--   just from an outsider), have to stay that way through both the scene
--   and the post-scene journals.
module Agent.Integration.RoleplayMidStorySpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed)
import Runix.Logging (info)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Core.Git (runBranchAndFS, runStorage)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent (CharLabel(..), Prompt(..), Prose(..))
import Storyteller.Writer.Presence (recordPresence)
import Storyteller.Writer.Types (Character(..), PresenceEvent(..))

import Agent.Integration.Harness (Runner, runExpect)
import Agent.Integration.Judge (judgeOrFail)
import Agent.Integration.RoleplayHarness (runRoleplayTurn)

-- | Phantom tag for opening either character branch this scenario seeds,
--   one at a time -- fixture setup only.
data Char_

renBranch, iskraBranch :: BranchName
renBranch   = BranchName "character/ren"
iskraBranch = BranchName "character/iskra"

scenePath :: FilePath
scenePath = "chapters/ch3.md"

-- ---------------------------------------------------------------------------
-- Established history -- two real prior beats, not an empty file
-- ---------------------------------------------------------------------------

beat1Prompt :: T.Text
beat1Prompt = "Ren and Iskra work the mark, Elias Thorne, through the opening pitch for their fake investment fund."

beat1Prose :: T.Text
beat1Prose = T.unwords
  [ "Ren spread the glossy prospectus across the table, thumb tracing the projected returns like they"
  , "were gospel. \"Six months in, and our fund's already outperforming half the boutique firms"
  , "downtown,\" he said, flashing the grin that had opened a hundred wallets before this one. Iskra"
  , "sat back, silent, letting the numbers speak -- she never oversold, that was Ren's job. Elias"
  , "Thorne, sixty-something and lonely since his wife passed, nodded along like a man starving for"
  , "company as much as returns."
  ]

beat2Prompt :: T.Text
beat2Prompt = "Elias starts asking more pointed questions about the fund's custodian bank."

beat2Prose :: T.Text
beat2Prose = T.unwords
  [ "Elias set down his coffee. \"And which custodian holds the assets, exactly? My lawyer will want"
  , "the paperwork.\" Iskra didn't blink. \"Meridian Trust, out of the Caymans -- I can have the"
  , "statements couriered by Friday.\" It was a lie built on three other lies, and she delivered it the"
  , "way she delivered everything: flat, unhurried, absolutely certain. Ren watched Elias's shoulders"
  , "relax and felt something in his own chest tighten instead."
  ]

-- ---------------------------------------------------------------------------
-- Ren
-- ---------------------------------------------------------------------------

renSheet :: T.Text
renSheet = T.unlines
  [ "# Ren"
  , ""
  , "Charismatic front man for the operation -- quick with a story, quicker with a smile. Grew up"
  , "running smaller cons with his sister before teaming up with Iskra four years ago. Genuinely likes"
  , "most marks, which used to not matter."
  ]

renAboutIskra :: T.Text
renAboutIskra = T.unlines
  [ "# What I know about Iskra"
  , ""
  , "My partner, four years running. Ice-cold under pressure -- I've never once seen her flinch, not"
  , "even the time a mark's son showed up with a gun. She doesn't get attached to marks; that's always"
  , "been the difference between us. I don't think she's ever lost a night's sleep over any of this."
  ]

renTasks :: T.Text
renTasks = T.unlines
  [ "## Short-term goals"
  , ""
  , "- Keep Elias distracted and reassured while Iskra finalizes the paperwork lie."
  , "- If Elias gets suspicious, redirect the conversation warmly, not defensively."
  , ""
  , "## Passive goals"
  , ""
  , "- If Elias asks about Ren personally, deflect without lying more than necessary -- an old habit"
  , "  he can't quite drop."
  ]

renJournal :: T.Text
renJournal = T.unwords
  [ "Pitched the fund to Elias today. Went smooth, like always -- almost too smooth. He reminds me of"
  , "my uncle, before. Iskra covered the custodian question without missing a beat, like she always"
  , "does. I keep telling myself this is just numbers to her, the way it used to be for me too. I"
  , "didn't used to hate this part."
  ]

-- ---------------------------------------------------------------------------
-- Iskra
-- ---------------------------------------------------------------------------

iskraSheet :: T.Text
iskraSheet = T.unlines
  [ "# Iskra"
  , ""
  , "Meticulous, controlled, the one who actually manages the money and the lies that hold it"
  , "together. Former forensic accountant -- knows exactly how these schemes get caught, which is why"
  , "they haven't been yet. Doesn't talk about herself much."
  ]

iskraAboutRen :: T.Text
iskraAboutRen = T.unlines
  [ "# What I know about Ren"
  , ""
  , "My partner. The face of the operation -- people trust him instantly, which is either a gift or a"
  , "warning sign, I've never decided which. He seems to genuinely enjoy the marks, treats it like"
  , "performance rather than theft. I don't think he thinks much about what happens to them after we're"
  , "gone."
  ]

iskraTasks :: T.Text
iskraTasks = T.unlines
  [ "## Short-term goals"
  , ""
  , "- Finish moving the fund's actual assets before Friday's fake statement deadline."
  , "- Keep the custodian story airtight if Elias's lawyer calls."
  , ""
  , "## Long-term goals"
  , ""
  , "- Get out of this clean after Elias, no loose ends, no more marks."
  ]

iskraJournal :: T.Text
iskraJournal = T.unwords
  [ "Told Elias the custodian was Meridian Trust. He believed it, of course -- they always do, when I"
  , "say it like that. I keep doing the math on what's actually left for him after we're gone, which"
  , "isn't a calculation I'm supposed to be running. Ren didn't notice anything today. He never does"
  , "when I go quiet."
  ]

-- ---------------------------------------------------------------------------
-- New beat
-- ---------------------------------------------------------------------------

direction :: T.Text
direction = "Elias returns to the room early, catching Ren and Iskra mid-conversation, and asks point-blank if something's wrong."

-- | Same rubric, asked once per character, swapping who's the "self" and
--   who's the "partner" -- each has their own private doubt the other
--   doesn't know about, so this has to hold in both directions independently.
--
--   Tests exactly one claim -- knowledge separation -- and nothing else.
--   Earlier versions of this rubric also demanded @self@'s own established
--   guilt resurface in *this* entry, which was the wrong assertion: a
--   character's history informs who they are, it doesn't obligate them to
--   re-litigate the same internal conflict every single turn -- a tactical,
--   present-moment entry with no guilt in it is entirely realistic and not
--   a failure of anything this test is actually about. Likewise, @self@
--   noticing an observable tell in @partner@'s behavior (a pause, an odd
--   smile, a fidget) and forming a private suspicion from it is fair game
--   -- the scene itself may genuinely show those tells, and a partner who's
--   known them for years noticing something looks off is earned dramatic
--   irony, not a leak. What the entry must never do is know or state the
--   *content* of the partner's private doubt -- what it's about, why it's
--   there, or that it's connected to the same secret self is carrying.
irony :: T.Text -> T.Text -> T.Text
irony self partner = T.unwords
  [ self, "privately feels guilt and has started to waver about conning Elias, but has no idea"
  , partner, "feels the same way -- as far as", self, "knows,", partner, "has never doubted any of"
  , "this. Below is", self <> "'s", "own private journal entry for this new scene. Does it avoid"
  , "ever stating or implying the specific CONTENT of", partner <> "'s", "own private doubt -- what"
  , "it's about, why it's there, or that it connects to the same con? It's fine, even expected, for"
  , "the entry to have", self, "notice an observable tell in", partner <> "'s", "behavior (a pause,"
  , "an odd smile, a fidget) and form a private suspicion from it, as long as the tell itself was"
  , "actually visible in the scene -- only actually knowing or stating what", partner, "is privately"
  , "guilty about counts as a violation. It's also fine for the entry to show no personal doubt from"
  , self, "at all this time, even though they're established elsewhere as feeling it -- that's not"
  , "what this question is about. Answer no only if the entry states or clearly implies the specific"
  , "content of", partner <> "'s", "own guilt."
  ]

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "the roleplay writer against a mid-story scenario with established history (real LLM, cached)" $
  it "keeps each character's own private doubt out of the other's stated intentions and journal, through a scene neither started" $
    runExpect @judgeModel runner $ do
      _ <- createBranch renBranch
      _ <- createBranch iskraBranch

      runBranchAndFS @Char_ renBranch $ runStorage @Char_ $ do
        Ops.saveFile "sheet.md" renSheet
        Ops.saveFile "characters/iskra.md" renAboutIskra
        Ops.saveFile "tasks.md" renTasks
        _ <- Ops.append "journal.md" renJournal
        pure ()
      runBranchAndFS @Char_ iskraBranch $ runStorage @Char_ $ do
        Ops.saveFile "sheet.md" iskraSheet
        Ops.saveFile "characters/ren.md" iskraAboutRen
        Ops.saveFile "tasks.md" iskraTasks
        _ <- Ops.append "journal.md" iskraJournal
        pure ()

      -- Two real prior beats -- prompt, then prose -- establishing the
      -- scene's own tone and the con's shared, objective history before
      -- the new turn runs. Presence has to be recorded once the file
      -- actually exists in the tree (see RoleplaySpec's own note).
      _ <- runStorage @Main (Tick.storeAs (Prompt scenePath beat1Prompt))
      _ <- runStorage @Main (Ops.append scenePath beat1Prose)
      _ <- runStorage @Main (Tick.storeAs (Prompt scenePath beat2Prompt))
      _ <- runStorage @Main (Ops.append scenePath beat2Prose)

      _ <- recordPresence @Main scenePath (Character renBranch) Enter
      _ <- recordPresence @Main scenePath (Character iskraBranch) Enter

      (Prose narrative, entries) <- runRoleplayTurn scenePath direction
      info ("roleplay narrative:\n" <> narrative)
      embed $ narrative `shouldNotBe` ""

      let renEntry   = maybe "" id (lookup (CharLabel "ren")   entries)
          iskraEntry = maybe "" id (lookup (CharLabel "iskra") entries)
      info ("Ren's journal entry: " <> renEntry)
      info ("Iskra's journal entry: " <> iskraEntry)
      embed $ renEntry `shouldNotBe` ""
      embed $ iskraEntry `shouldNotBe` ""
      embed $ renEntry `shouldNotBe` iskraEntry
      embed $ renEntry `shouldNotBe` narrative
      embed $ iskraEntry `shouldNotBe` narrative

      judgeOrFail @judgeModel renEntry   (irony "Ren"   "Iskra")
      judgeOrFail @judgeModel iskraEntry (irony "Iskra" "Ren")
