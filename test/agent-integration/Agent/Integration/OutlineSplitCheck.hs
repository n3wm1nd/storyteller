{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Shared checker behind every no-tool-call split variant
--   ('Storyteller.Writer.Agent.Outline.splitOutlineFreeform',
--   'Storyteller.Writer.Agent.Outline.splitOutlineBulk', and any future
--   one): same messy fixtures, same structural checks, same judge question.
--   They all produce the exact same shape of result -- a list of
--   'Storyteller.Writer.Agent.Outline.ChapterBeats', i.e. a bunch of files
--   -- so there's no reason for the *check* to differ by mechanism, only
--   the label and which split function is under test. See @FINDINGS.md@
--   for what comparing their results has actually turned up.
module Agent.Integration.OutlineSplitCheck
  ( SplitFn
  , splitAgainstMessyOutlines
  ) where

import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Text as T
import Test.Hspec

import Polysemy (Members, Sem, embed)
import Polysemy.Fail (Fail)
import Runix.Logging (Logging, info)
import UniversalLLM (HasTools, ProviderOf, SupportsSystemPrompt)

import Agent.Integration.Harness (Runner, runExpect)
import Agent.Integration.Judge (judgeOrFail)
import Agent.Integration.OutlineSplitQualitySpec (messyOutlines)
import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Core.Prompt (PromptStorage)
import Storyteller.Writer.Agent (ContextBlock(..))
import Storyteller.Writer.Agent.Outline (BeatSheet(..), ChapterBeats(..), OutlineDoc(..))

-- | The shape every no-tool-call split variant has in common --
--   'Storyteller.Writer.Agent.Outline.splitOutlineFreeform' and
--   'Storyteller.Writer.Agent.Outline.splitOutlineBulk' both already have
--   exactly this type.
type SplitFn
  =  forall r. (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => [ContextBlock] -> OutlineDoc -> Sem r [ChapterBeats]

splitAgainstMessyOutlines
  :: forall judgeModel
  .  (HasTools judgeModel, SupportsSystemPrompt (ProviderOf judgeModel))
  => String    -- ^ describe label, e.g. "splitOutlineBulk against ..."
  -> SplitFn   -- ^ the split variant under test
  -> Runner judgeModel -> Spec
splitAgainstMessyOutlines label splitFn runner = describe label $
  mapM_ (\(name, outline) -> it name (checkOutline outline)) messyOutlines
  where
    checkOutline outline = runExpect @judgeModel runner $ do
      sheets <- splitFn [] (OutlineDoc outline)
      info $ "split step: " <> T.pack (show (length sheets)) <> " sheet(s)"

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
