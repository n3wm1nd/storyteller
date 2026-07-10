{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Stress 'Storyteller.Writer.Agent.Outline.splitOutlineAgent'\'s
--   @emit_beat_sheet@ tool call against outlines shaped like something a
--   real user would actually paste in, not the clean, agent-generated
--   pitch 'Agent.Integration.JourneySpec' exercises. 'JourneySpec' always
--   feeds the split step a beat sheet its own outline step just wrote, in
--   the same run, on the same model -- so its formatting is already
--   whatever that model finds easiest to echo back. A user's own outline
--   carries embedded quotes, dashes, nested lists, nonstandard headings, and
--   the occasional code fence, none of which 'splitOutlineAgent' controls.
--
--   The point of running these isn't a green checkmark -- there's a
--   separate, mocked suite (@storyteller-test@) for plumbing correctness.
--   It's to see, against the real model these roles are actually configured
--   to use, which shapes of input make @emit_beat_sheet@ come back
--   genuinely malformed (unparseable JSON -- see
--   'Agent.Integration.Harness.assertToolCallBudget'), or which shapes
--   produce content that doesn't faithfully cover its own chapter (see the
--   judge check below), so the prompt and instructions can be tuned against
--   real failure modes instead of guessed at. Deliberately does *not* flag
--   a call just for containing something backslash-shaped: fixtures here
--   invite the model to invent plausible technical color, so a check that
--   penalized it for doing so would be testing against its own instructions
--   (see @../PLAN.md@ on why 'Agent.Integration.ToolCallQuality.escapingArtifacts'
--   isn't wired into this spec's assertions).
--
--   Deliberately answers a *different* question than
--   @Agent.Integration.OutlineSplitDefaultsSpec@ (see @../PLAN.md@): this
--   spec nudges the model toward concise beat sheets via a
--   'Storyteller.Core.Prompt.PromptStorage' override
--   ('Agent.Integration.Harness.withPromptOverride') on purpose -- it's
--   checking that the *pipeline* handles messy input correctly (tool-call
--   format, path conventions, content fidelity), not measuring whether the
--   *shipped, unmodified* settings hold up at full scale. That's what
--   @OutlineSplitDefaultsSpec@ is for, without the nudge.
module Agent.Integration.OutlineSplitQualitySpec (spec, messyOutlines) where

import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed)
import Runix.Logging (info)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import Agent.Integration.Harness (Runner, assertToolCallBudget, runExpect, withPromptOverride)
import Agent.Integration.Judge (judgeOrFail)
import Storyteller.Core.LLM.Role (AgentModel)
import Storyteller.Core.Prompt (Prompt(..))
import Storyteller.Writer.Agent.Outline (BeatSheet(..), ChapterBeats(..), OutlineDoc(..), splitOutlineAgent)

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "splitOutlineAgent against realistically messy, user-authored outlines" $
  mapM_ (\(name, outline) -> it name (checkOutline outline)) messyOutlines
  where
    checkOutline outline = runExpect @judgeModel runner $ do
      -- Budget 0: this spec exists specifically to measure whether the
      -- model gets the call right without needing a retry -- see
      -- 'Agent.Integration.Harness.assertToolCallBudget's Haddock and
      -- @../PLAN.md@. Reaching the next line at all means the wire format
      -- was fine; everything below is about content, not format. The
      -- concision nudge (below) exists so a busy model still finishing
      -- comfortably under budget is the interesting signal, not "ran out of
      -- room for a chapter it wasn't trying to keep short."
      (sheets, turns) <- withPromptOverride "agent.outline.split.instructions" conciseInstructions
        (assertToolCallBudget @AgentModel 0 (splitOutlineAgent [] (OutlineDoc outline)))
      info $ "split step: " <> T.pack (show (length turns)) <> " turn(s), "
           <> T.pack (show (length sheets)) <> " sheet(s)"

      embed $ do
        -- At least one chapter should have come out the other side however
        -- messy the input was -- an empty result means the model gave up
        -- entirely, which is its own kind of failure worth seeing.
        sheets `shouldSatisfy` (not . null)
        -- Same WRITER.md path convention 'Agent.Integration.JourneySpec'
        -- checks -- a beat sheet that doesn't land at @chapters/*.outline.md@
        -- would silently break the next stage's own path-based lookup.
        mapM_ (\(ChapterBeats path _) ->
                 path `shouldSatisfy` \p -> "chapters/" `isPrefixOf` p && ".outline.md" `isSuffixOf` p)
              sheets

      -- Content fidelity, not just structure: format reliability is already
      -- enforced above, but a beat sheet that parsed fine can still garble
      -- or swap chapters under irregular input formatting -- structural
      -- checks alone can't catch that, hence the judge (see @../PLAN.md@).
      let artifact = T.unlines $
            [ "Outline:", "", outline, "", "---", "", "Emitted beat sheets, in order:" ] <>
            concatMap (\(ChapterBeats path (BeatSheet sheet)) -> ["", T.pack path <> ":", sheet]) sheets
      judgeOrFail @judgeModel artifact $ T.unwords
        [ "Above is a messy, informally-formatted story outline, then the beat"
        , "sheets a splitting step emitted from it. Do the beat sheets"
        , "collectively cover the outline's chapters faithfully, in the"
        , "outline's own order, without garbling, swapping, or dropping any"
        , "chapter's content? Answer no if a beat sheet's content doesn't"
        , "match its own chapter, or the chapter count/order is clearly"
        , "wrong. Keep your reason to one sentence."
        ]

-- | Overrides @agent.outline.split.instructions@ (see
--   'Storyteller.Writer.Agent.Outline.defaultSplitInstructions') for the
--   scenarios in this spec: still one heading, logistics, emotional turn,
--   and length note per beat -- the shape a real beat sheet has to have for
--   the fidelity checks below to mean anything -- just shorter, so the
--   scenario stays fast and comfortably inside budget regardless of what
--   'Storyteller.Writer.Agent.Outline.defaultSplitConfig'\'s @MaxTokens@
--   happens to be. Not a stand-in for testing the real default -- see
--   @Agent.Integration.OutlineSplitDefaultsSpec@ for that.
conciseInstructions :: Prompt
conciseInstructions = Prompt $ T.unwords
  [ "Call emit_beat_sheet once per chapter, in reading order. Keep each beat"
  , "to 1-2 sentences under its heading -- still naming what happens, who's"
  , "there, the emotional turn, and a rough length, just briefly. This is a"
  , "test run, not a real planning session; concise but complete beats are"
  , "more useful here than elaborate ones."
  ]

-- | Fixed, named outlines -- fixed for the same reason
-- 'Agent.Integration.Journey.storyPremise' is: so the on-disk response cache
-- actually hits on repeat runs. Each is meant to isolate one shape of
-- "real outlines aren't clean" rather than piling every irregularity into
-- one fixture, so a failure on one points at what to fix.
messyOutlines :: [(String, T.Text)]
messyOutlines =
  [ ("outline with quoted dialogue and mixed dash/quote styles", quotesAndDashes)
  , ("outline written as nested bullet lists instead of prose", nestedBullets)
  , ("outline with a non-chapter lead-in and bracketed author's asides", codeFenceAndBackslashes)
  ]

quotesAndDashes :: T.Text
quotesAndDashes = T.unlines
  [ "Chapter One -- \"Nothing Ever Happens Here\""
  , "Marta doesn't believe in signs -- she's said so, more than once, to anyone"
  , "who'll listen. \"It's just weather,\" she tells her neighbor, watching the"
  , "sky go the color of a bruise. Her neighbor isn't so sure -- he's seen this"
  , "before, back in '09, and he doesn't like it. \"You'll want to board up the"
  , "windows,\" he says -- not a suggestion."
  , ""
  , "Chapter Two: The Storm That Wasn't a Storm"
  , "By midnight it's clear this isn't weather at all -- it's something else,"
  , "something that hums instead of roars. Marta's \"just weather\" is looking"
  , "less and less convincing, even to her. She goes outside anyway -- against"
  , "everyone's advice, including her own -- because she has to know."
  , ""
  , "Chapter Three -- What the Neighbor Knew"
  , "Turns out the neighbor's \"back in '09\" wasn't a throwaway line -- he'd"
  , "seen the lights before, and he'd buried what he found under the shed."
  , "Marta doesn't want to believe him. She digs anyway."
  ]

nestedBullets :: T.Text
nestedBullets = T.unlines
  [ "# Outline"
  , ""
  , "- Chapter 1"
  , "  - Dana inherits a lighthouse she's never heard of"
  , "    - No family ever mentioned it"
  , "    - The deed is real, notarized, decades old"
  , "  - She drives out to see it"
  , "    - It's smaller than the photos"
  , "    - The light still works, somehow, with no power connected"
  , "- Chapter 2"
  , "  - The town doesn't want her there"
  , "    - Nobody says why, exactly"
  , "    - **Old Peters** keeps warning her off, vaguely"
  , "  - She finds a logbook in the keeper's room"
  , "    - Entries stop abruptly in 1974"
  , "    - The last one is just: *it's still on, don't ask why*"
  , "- Chapter 3"
  , "  - Dana starts keeping the log herself"
  , "    - Nights get stranger the longer she stays"
  , "    - Ships that shouldn't be there, gone by morning"
  , "  - She finally asks Old Peters directly"
  , "    - He tells her everything -- more than she wanted"
  ]

-- | A tech-mystery premise, told the way a real user actually drafts an
--   outline: a lead-in blurb before the first chapter heading (not itself
--   part of any chapter), and bracketed asides to themselves scattered
--   through it. An earlier version of this fixture also embedded a literal
--   code fence with a raw file path and JSON blob as the diegetic "clue" --
--   turned out that was too adversarial a case to draw any conclusion from
--   (see @../PLAN.md@ on the outline/output literalism contract): quoting a
--   plot-critical technical detail verbatim is *correct* behaviour once the
--   outline hands it to the model as literal story content, so a beat sheet
--   containing a raw backslash there wasn't a bug at all. This keeps the
--   same mystery (a strange inherited file, a phone call) but describes it
--   in ordinary prose, the way an outline realistically would.
codeFenceAndBackslashes :: T.Text
codeFenceAndBackslashes = T.unlines
  [ "A quiet inheritance mystery, tech-flavored -- think family secret meets"
  , "cold open. Keep the reveal slow. {working title: FINAL_V2, might change}"
  , ""
  , "Chapter 1: The Backup"
  , "Priya finds an old external drive labeled DAD - DO NOT FORMAT while"
  , "clearing out her father's study. The one file on it is a script he wrote"
  , "years ago, still pointing at a folder on his old machine that no longer"
  , "exists. {maybe cut the drive-cleaning montage if ch1 runs long} She has"
  , "no idea what he was trying to preserve."
  , ""
  , "Chapter 2: Running It Anyway"
  , "Against her better judgement she runs the script in a sandboxed VM."
  , "It prints one cryptic status line before halting -- something about"
  , "being unfinished, and an instruction to call a number -- then stops."
  , "{echo the \"unfinished\" word choice later, it should land harder in ch3}"
  , "The number turns out to be disconnected. She calls it anyway, and"
  , "someone picks up."
  , ""
  , "Chapter 3: The Number"
  , "The voice on the line knows her father's name, and knows about the"
  , "unfinished project. What it wants from Priya isn't the file. It's a"
  , "promise. {ending should feel like a door opening, not closing -- setup"
  , "for book 2}"
  ]
