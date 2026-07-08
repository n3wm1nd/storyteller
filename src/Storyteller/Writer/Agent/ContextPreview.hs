{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Context preview: given a command's declared slots, resolve each one's
-- filter into the files that would actually populate it, without running
-- any agent or LLM call.
--
-- A 'ContextSlot' is what a command contributes on its own — its label and
-- its 'ContextMode' (Ambient vs OnDemand) are fixed by that command's own
-- code, mirroring how e.g. 'Storyteller.Writer.Agent.Write.writeAgent'
-- always injects character context ambiently and how
-- 'Storyteller.Writer.Agent.Chat.chatAgent' always exposes branch files as
-- on-demand tool reads, never the other way around. The one part a client
-- gets to configure is 'csFilter' — which files populate that slot right
-- now.
--
-- Deliberately just a preview: no interceptor is installed anywhere, and no
-- generation agent consults this module's output. Wiring an agent to
-- actually honour a submitted filter (rather than reading everything, as
-- 'Storyteller.Writer.Agent.Continuation.gatherFileContext'/
-- 'Storyteller.Writer.Agent.CharContext.readCharFiles' do today) is future
-- work, out of scope here.
module Storyteller.Writer.Agent.ContextPreview
  ( ContextMode(..)
  , PathFilter(..)
  , ContextSlot(..)
  , ContextEntry(..)
  , ContextSlotPreview(..)
  , buildSlotPreview
  , buildPreview
  ) where

import Data.Maybe (listToMaybe)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.FilePath.Glob as Glob

import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, glob, readFile)

import Prelude hiding (readFile)

-- | How a slot is delivered to whichever agent/subagent consumes it — fixed
--   by the command that declares the slot, never client-configurable.
data ContextMode = Ambient | OnDemand
  deriving (Show, Eq)

-- | Which files populate a slot. Glob patterns, same syntax
--   'Runix.Tools.glob'/'Storyteller.Core.Git's @Glob@ filesystem op already
--   use — no bespoke pattern language. Empty 'pfInclude' means "everything".
data PathFilter = PathFilter
  { pfInclude :: [T.Text]
  , pfExclude :: [T.Text]
  } deriving (Show, Eq)

-- | One named context slot, as a command declares it: a label the command
--   chose (e.g. @"character:alice-chen"@, @"branch-files"@), its fixed
--   delivery mode, and the (client-supplied) filter selecting its files.
data ContextSlot = ContextSlot
  { csLabel  :: T.Text
  , csMode   :: ContextMode
  , csFilter :: PathFilter
  } deriving (Show, Eq)

-- | One matched file's preview. 'Ambient' slots show the full content, the
--   same as what an agent would actually be handed; 'OnDemand' slots show
--   only a blurb, since the real thing an on-demand slot hands over is a
--   tool the model may or may not call, not injected text.
--
--   'ceIncluded' is False for a file the include patterns matched but the
--   exclude patterns then dropped (or, inverted, everything the include-only
--   list didn't pick) — kept in the list rather than removed so a client can
--   show it shaded-out in place instead of just disappearing, per the design
--   discussion on the Agents tab's context preview. Excluded entries never
--   load content/blurb: nothing reads a file that won't be sent anyway.
data ContextEntry = ContextEntry
  { cePath      :: FilePath
  , ceContent   :: Maybe T.Text
  , ceBlurb     :: Maybe T.Text
  , ceIncluded  :: Bool
  } deriving (Show, Eq)

data ContextSlotPreview = ContextSlotPreview
  { cspLabel   :: T.Text
  , cspMode    :: ContextMode
  , cspEntries :: [ContextEntry]
  } deriving (Show, Eq)

-- | Resolve one slot: always list every file the slot could possibly see
--   ("**/*"), then mark each one included or not per the filter, rather
--   than using 'pfInclude'\/'pfExclude' to narrow which paths are listed at
--   all. That keeps the same full listing (and thus the same shaded-not-
--   missing behaviour, see 'ContextEntry's 'ceIncluded') whichever way the
--   filter is pointed: a non-empty 'pfInclude' means "only these survive"
--   (everything else still appears, just excluded), a non-empty
--   'pfExclude' means "only these are dropped" — the two are never both
--   populated at once (one client-side filter just flips which list it
--   fills, see context-source.tsx's 'send'). Only survivors get their
--   content/blurb loaded, per the slot's mode. Same 'FileSystem'/
--   'FileSystemRead' pair 'Storyteller.Writer.Agent.Continuation.gatherFileContext'
--   already requires — this is a third consumer of that same read surface,
--   not a new capability.
buildSlotPreview
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => ContextSlot
  -> Sem r ContextSlotPreview
buildSlotPreview (ContextSlot label mode (PathFilter includes excludes)) = do
  matched <- glob @project "/" "**/*"
  let includeGlobs = map (Glob.compile . T.unpack) includes
      excludeGlobs = map (Glob.compile . T.unpack) excludes
      normalized p = dropWhile (== '/') p
      isIncluded p
        | not (null includes) = any (`Glob.match` normalized p) includeGlobs
        | otherwise            = not (any (`Glob.match` normalized p) excludeGlobs)
  entries <- mapM (loadEntry @project mode . (\p -> (p, isIncluded p))) (List.nub (List.sort matched))
  return (ContextSlotPreview label mode entries)

loadEntry
  :: forall project r
  .  Members '[FileSystemRead project, Fail] r
  => ContextMode -> (FilePath, Bool) -> Sem r ContextEntry
loadEntry _ (path, False) = return (ContextEntry path Nothing Nothing False)
loadEntry mode (path, True) = do
  text <- TE.decodeUtf8 <$> readFile @project path
  return $ case mode of
    Ambient  -> ContextEntry path (Just text) Nothing True
    OnDemand -> ContextEntry path Nothing (Just (blurb text)) True

-- | First non-blank line, trimmed to a short teaser — enough to recognise a
--   file by without loading its full content, since 'OnDemand' entries are
--   never injected wholesale.
blurb :: T.Text -> T.Text
blurb t = T.take 140 $ maybe "" id $
  listToMaybe [ line | l <- T.lines t, let line = T.strip l, not (T.null line) ]

buildPreview
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => [ContextSlot]
  -> Sem r [ContextSlotPreview]
buildPreview = mapM (buildSlotPreview @project)
