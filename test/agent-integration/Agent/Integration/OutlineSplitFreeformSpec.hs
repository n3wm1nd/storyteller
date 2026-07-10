{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The experiment 'Storyteller.Writer.Agent.Outline.splitOutlineFreeform'
--   exists to run: same messy fixtures
--   'Agent.Integration.OutlineSplitQualitySpec' uses, same content-fidelity
--   judge check, but driving the split as a plain conversation instead of a
--   tool-call loop -- so a run of this spec is directly comparable to a run
--   of that one, model for model, fixture for fixture. See
--   'Storyteller.Writer.Agent.Outline.splitOutlineFreeform's Haddock and
--   @../PLAN.md@ for why this exists at all: a raw probe against
--   gpt-oss-20b found the tool-call loop reliably duplicating/omitting/
--   garbling chapters where a plain-conversation prompt got the same
--   breakdown perfectly right.
--
--   No tool-call-format checks here (there's no tool call to check) --
--   'Agent.Integration.Harness.assertToolCallBudget'\/'escapingArtifacts'
--   don't apply. Just: did it produce the right number of chapters, at the
--   right paths, faithfully covering the outline?
module Agent.Integration.OutlineSplitFreeformSpec (spec) where

import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Text as T
import Test.Hspec

import Polysemy (embed)
import Runix.Logging (info)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import Agent.Integration.Harness (Runner, runExpect)
import Agent.Integration.Judge (judgeOrFail)
import Agent.Integration.OutlineSplitQualitySpec (messyOutlines)
import Storyteller.Writer.Agent.Outline (BeatSheet(..), ChapterBeats(..), OutlineDoc(..), splitOutlineFreeform)

spec
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => Runner judgeModel -> Spec
spec runner = describe "splitOutlineFreeform against the same messy outlines, no tool-call loop" $
  mapM_ (\(name, outline) -> it name (checkOutline outline)) messyOutlines
  where
    checkOutline outline = runExpect @judgeModel runner $ do
      sheets <- splitOutlineFreeform [] (OutlineDoc outline)
      info $ "split step (freeform): " <> T.pack (show (length sheets)) <> " sheet(s)"

      embed $ do
        sheets `shouldSatisfy` (not . null)
        mapM_ (\(ChapterBeats path _) ->
                 path `shouldSatisfy` \p -> "chapters/" `isPrefixOf` p && ".outline.md" `isSuffixOf` p)
              sheets

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
