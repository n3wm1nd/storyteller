{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | A whole user session, replayed against real agents: outline a story,
--   split it into per-chapter beat sheets, then write each chapter from its
--   beat sheet -- the same three requests a Writer-tab user issues over the
--   WebSocket, in the same order, against the same file conventions
--   (WRITER.md). 'runJourney' is the single entry point; anything that wants
--   a populated branch (filesystem plus tick history) to test against calls
--   it and gets the same result back on a cache hit, since every prompt
--   below is fixed.
--
--   Deliberately doesn't call through 'Server.Writer.File' -- that module's
--   @chatWriter@\/@chatSplitOutline@ are pinned to
--   'Storyteller.Core.Runtime.StoryModel' via 'Server.Core.Run.SessionEffects'
--   (the app has exactly one configured model in production), which would
--   defeat this suite's whole point of swapping @STORY_MODEL@ per run (see
--   'Agent.Integration.Harness'). Instead this replicates the same two
--   moves those handlers make -- store the prompt tick, gather context,
--   call the agent, split and append the result -- generically over
--   @storyModel@. 'writeChat' is that replica of
--   'Server.Writer.File.chatWriter'\'s no-flow-tick branch; the outline
--   split and chapter-generation steps below call it directly rather than
--   going through 'Server.Writer.File.chatSplitOutline', which differs from
--   'writeChat' only in swapping 'splitOutlineAgent' for 'writeAgent'.
module Agent.Integration.Journey
  ( JourneyResult(..)
  , storyPremise
  , runJourney
  ) where

import Control.Monad (forM)
import Data.List (isSuffixOf, sort)
import qualified Data.Text as T

import Polysemy (Members, Sem)
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, listAllFiles)
import Runix.LLM (LLM)
import Runix.Logging (Logging, info)
import UniversalLLM (HasTools, ModelConfig, ProviderOf, SupportsSystemPrompt)

import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Common.Splitter (Splitter, splitAtoms)
import Storyteller.Core.Git (BranchOp, BranchTag, runStorage)
import Storyteller.Core.Prompt (PromptStorage)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Writer.Agent (Instruction(..), Prompt(..), Prose(..))
import Storyteller.Writer.Agent.ContextFilter (hideBinaryFiles)
import Storyteller.Writer.Agent.Continuation (gatherFileContext)
import Storyteller.Writer.Agent.Outline (BeatSheet(..), ChapterBeats(..), OutlineDoc(..), splitOutlineAgent)
import Storyteller.Writer.Agent.Write (writeAgent)

-- | Every effect one journey step needs -- exactly 'Server.Writer.File'\'s
--   own imports, minus 'Server.Core.Run.SessionEffects'\' fixed
--   @LLM StoryModel@ (generalised to @LLM storyModel@ here) and minus
--   @Random@\/@Error String@, neither of which any step below touches.
type JourneyEffects storyModel r =
  ( HasTools storyModel, SupportsSystemPrompt (ProviderOf storyModel)
  , Members '[ LLM storyModel, PromptStorage, Splitter, Logging
             , StoryStorage, BranchOp Main
             , FileSystem      (BranchTag Main)
             , FileSystemRead  (BranchTag Main)
             , FileSystemWrite (BranchTag Main)
             , Fail
             ] r
  )

-- | The three requests a Writer-tab session issues, and what each produced.
data JourneyResult = JourneyResult
  { jrOutline  :: T.Text            -- ^ full contents of @outline.md@
  , jrChapters :: [ChapterBeats]    -- ^ beat sheets 'splitOutlineAgent' emitted, reading order
  , jrProse    :: [(FilePath, T.Text)] -- ^ (chapter path, generated prose) per beat sheet, same order
  , jrFiles    :: [FilePath]        -- ^ every path actually in the branch once the session is done,
                                     --   sorted -- read straight off the filesystem, independent of
                                     --   'jrChapters'\/'jrProse' above, so a caller can check what
                                     --   landed on disk rather than trusting what the agents claimed
  } deriving Show

-- | The pitch a user types to kick off a brand new story. Fixed, so a
--   'runJourney' call is the same request every time -- what makes the
--   on-disk LLM response cache ('Agent.Integration.Harness') actually hit.
storyPremise :: T.Text
storyPremise = T.unwords
  [ "Write a five-chapter story outline for a science-fiction comedy in the"
  , "vein of The Hitchhiker's Guide to the Galaxy: an ordinary, unprepared"
  , "protagonist is swept off Earth just ahead of its destruction and dragged"
  , "across an absurd, indifferent galaxy. One heading per chapter, with a"
  , "few sentences under each covering what happens. Output only the outline."
  ]

-- | Outline, split, write -- in that order, against a single branch. See
--   the module Haddock for why this doesn't call through
--   'Server.Writer.File'.
runJourney
  :: forall storyModel r
  .  JourneyEffects storyModel r
  => [ModelConfig storyModel]
  -> Sem r JourneyResult
runJourney configs = do
  info "journey: generating outline.md"
  outline <- writeChat @storyModel configs "outline.md" storyPremise

  info "journey: splitting outline into chapter beat sheets"
  (_, outlineCtx) <- gatherFileContext @(BranchTag Main) [] "outline.md"
  sheets <- splitOutlineAgent @storyModel configs outlineCtx (OutlineDoc outline)
  mapM_ (\(ChapterBeats path (BeatSheet body)) -> appendGenerated path body) sheets
  info $ "journey: got " <> T.pack (show (length sheets)) <> " beat sheet(s)"

  info "journey: writing each chapter from its beat sheet"
  chapters <- forM sheets $ \(ChapterBeats sheetPath _) -> do
    let chapterPath = chapterPathFor sheetPath
    prose <- writeChat @storyModel configs chapterPath (chapterInstruction sheetPath)
    return (chapterPath, prose)

  files <- logFileTree @(BranchTag Main)
  return (JourneyResult outline sheets chapters files)

-- | List every path in the branch, sorted, and log it -- a plain filesystem
--   listing, no LLM call, just visibility into what the three requests above
--   actually left behind. Run once at the end of 'runJourney' rather than
--   after each step, so a caller sees the finished tree in one place.
logFileTree
  :: forall project r
  .  Members '[FileSystem project, Logging, Fail] r
  => Sem r [FilePath]
logFileTree = do
  paths <- sort <$> listAllFiles @project "/"
  info $ T.unlines ("journey: final file tree:" : map T.pack paths)
  return paths

-- | Replica of 'Server.Writer.File.chatWriter'\'s no-flow-tick branch:
--   store the prompt as a tick, gather the target file's existing content
--   plus every other branch file as context (binary files hidden, same as
--   production), run 'writeAgent', then split and append the result. No
--   pinned character branches or extra context items -- neither journey
--   step here has any.
writeChat
  :: forall storyModel r
  .  JourneyEffects storyModel r
  => [ModelConfig storyModel] -> FilePath -> T.Text -> Sem r T.Text
writeChat configs path prompt = do
  _ <- runStorage @Main (Tick.storeAs (Prompt path prompt))
  (existing, fileCtx) <- hideBinaryFiles @(BranchTag Main) @Main (gatherFileContext @(BranchTag Main) [] path)
  Prose generated <- writeAgent @storyModel configs existing fileCtx (Instruction prompt) []
  appendGenerated path generated
  return generated

appendGenerated
  :: Members '[Splitter, StoryStorage, BranchOp Main, Fail] r
  => FilePath -> T.Text -> Sem r ()
appendGenerated path content =
  mapM_ (\c -> runStorage @Main (Ops.append path c)) =<< splitAtoms content

-- | @chapters/ch1.outline.md@ -> @chapters/ch1.md@ (WRITER.md convention;
--   the inverse of 'Server.Writer.File.beatSheetPathFor'). A beat sheet path
--   that doesn't follow the convention is a bug in 'splitOutlineAgent'
--   itself, not something this test should paper over -- left un-stripped
--   so a broken path shows up as a mis-shaped file rather than being
--   silently coerced.
chapterPathFor :: FilePath -> FilePath
chapterPathFor path
  | suffix `isSuffixOf` path = take (length path - length suffix) path <> ".md"
  | otherwise                = path
  where suffix = ".outline.md"

-- | Frame the chapter-writing request the same way a user, looking at a
--   freshly split beat sheet, would type it -- naming the beat sheet so the
--   model knows which of the many context blocks 'gatherFileContext' handed
--   it to follow, without dictating a word count or driving style ('writeAgent'
--   already fixes the length hint).
chapterInstruction :: FilePath -> T.Text
chapterInstruction sheetPath = T.pack $
  "Write this chapter's prose, following its beat sheet at " <> sheetPath
  <> ", which is included in the context below. Realize every beat in order. \
     \Output only the chapter's prose."
