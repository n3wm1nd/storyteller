{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Story-wide (not per-character) context: the freeform lore a user
-- hand-authors (notes, places, events, world-building, the whole-story
-- outline, a chapter's own beat sheet) versus the one reserved file meant
-- as standing instruction to the model (a style guide). Both are drawn from
-- the same 'isWorldContextEligible' file set by a single path-convention
-- split, not two independent classification schemes -- 'style.md' at the
-- branch root is 'SystemContext', everything else eligible is 'WorldLore'.
--
-- Lives at the 'Core.StoreT' level directly, same reasoning as
-- 'Storyteller.Writer.Agent.CharContext.charSummaryWithJournal': a caller
-- opens the branch scope once and passes 'worldContextOf' straight to
-- 'Storyteller.Core.Git.runStorage', one dispatch for both halves rather
-- than two.
module Storyteller.Writer.Agent.WorldContext
  ( WorldLore(..)
  , SystemContext(..)
  , isSystemContextPath
  , isWorldContextEligible
  , worldContextOf
  ) where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath (takeDirectory, takeFileName)

import qualified Storage.Core as Core
import qualified Storage.FS as FS

import Storyteller.Writer.Agent (ContextBlock(..), renderEmbeddedFile)
import qualified Storyteller.Writer.Library as Library
import Storyteller.Writer.Lore (isNotScratchOrCharacterFile)

-- | The one reserved path a project's style guide, voice notes, and other
--   direct-to-the-model instructions live at -- root-level @style.md@,
--   mirroring the @sheet.md@\/@journal.md@ convention 'isWorldContextEligible'
--   already special-cases for a character branch. Everything else
--   'isWorldContextEligible' accepts is 'WorldLore' instead.
isSystemContextPath :: FilePath -> Bool
isSystemContextPath path = takeFileName path == "style.md" && takeDirectory path == "."

-- | Which files 'worldContextOf' treats as story-wide context (root
--   @style.md@ aside -- 'isSystemContextPath' pulls that one back out into
--   'SystemContext' separately). Deliberately broader than
--   'Storyteller.Writer.Lore.isLoreEligible': that predicate also excludes
--   anything 'Storyteller.Writer.Library.classifyPath' calls
--   'Library.UnitOutline' (the whole-story @outline.md@, or a chapter's own
--   @{stem}.outline.md@ beat sheet) because it backs the @\/lore@ codex UI,
--   where an outline already has its own home in the Library tab and
--   showing it again there would be a duplicate. Generation context has no
--   such duplicate to avoid -- nothing else ever hands the model the
--   whole-story outline or an unreferenced beat sheet, so plain files like
--   that belong here the same as any other hand-authored note. The one kind
--   excluded here is a real chapter ('Library.Unit') -- that already has
--   its own dedicated, ordered channel
--   ('Storyteller.Writer.Agent.ChapterContext.earlierChaptersOf'), so
--   including it here too would show it a second time, unordered.
isWorldContextEligible :: FilePath -> Bool
isWorldContextEligible path =
  Library.classifyPath path /= Library.Unit && isNotScratchOrCharacterFile path

-- | The story's freeform reference material -- notes, places, events, beat
--   sheets, the whole-story outline, anything a user hand-authored that
--   isn't a chapter, isn't chat scratch, and isn't the reserved
--   'isSystemContextPath' style guide. Meant to be read as reference
--   material the model reasons over, not instruction -- belongs early in a
--   chapter's history, ahead of the chapter prose itself, distinct from
--   'SystemContext' which belongs in the system prompt instead.
newtype WorldLore = WorldLore [ContextBlock]
  deriving (Show, Eq)

-- | Direct, standing instructions to the model -- style guide, voice,
--   recurring constraints a project wants applied to every generation.
--   Meant to be appended to an agent's own system prompt: the agent's own
--   "you are a..." framing always comes first and is never overridden
--   here, and whether an agent appends this at all is that agent's own
--   call -- the chat agent, for instance, likely wouldn't want a prose
--   style guide folded into its persona.
newtype SystemContext = SystemContext [ContextBlock]
  deriving (Show, Eq)

-- | Both halves of the story-wide split, one dispatch. Ordered within each
--   half by 'Library.naturalKey' -- the same numeric-aware comparator the
--   Library tab itself sorts chapters by -- rather than plain string order,
--   since a beat sheet is now eligible lore alongside everything else
--   (see 'isWorldContextEligible') and @ch2.outline.md@\/@ch10.outline.md@
--   need to land in reading order, not @ch10@ before @ch2@.
worldContextOf :: forall m. Core.StoreM m => Core.StoreT m (WorldLore, SystemContext)
worldContextOf = do
  files <- List.sortOn (Library.naturalKey . T.pack) . filter isWorldContextEligible <$> FS.list
  let (styleFiles, loreFiles) = List.partition isSystemContextPath files
  lore    <- WorldLore <$> mapM readBlock loreFiles
  sysCtx  <- SystemContext <$> mapM readBlock styleFiles
  return (lore, sysCtx)
  where
    readBlock path = do
      content <- TE.decodeUtf8 <$> Core.readFile path
      return $ ContextBlock $ renderEmbeddedFile path content
