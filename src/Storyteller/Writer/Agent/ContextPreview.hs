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
-- actually honour a submitted filter (rather than reading everything, the
-- way @context.main@'s own DSL text and
-- 'Storyteller.Writer.Agent.CharContext.readCharFiles' do today) is future
-- work, out of scope here.
module Storyteller.Writer.Agent.ContextPreview
  ( ContextMode(..)
  , ContextSlot(..)
  , ContextEntry(..)
  , ContextSlotPreview(..)
  , buildSlotPreview
  , buildPreview
  , blurb
  ) where

import Data.Maybe (listToMaybe)
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, glob, readFile)

import Storyteller.Writer.Agent.ContextFilter (ContextLayout, classifyPath)

import Prelude hiding (readFile)

-- | How a slot is delivered to whichever agent/subagent consumes it — fixed
--   by the command that declares the slot, never client-configurable.
data ContextMode = Ambient | OnDemand
  deriving (Show, Eq)

-- | One named context slot, as a command declares it: a label the command
--   chose (e.g. @"character:alice-chen"@, @"branch-files"@), its fixed
--   delivery mode, and the (client-supplied) bucket-picker layout selecting
--   and ordering its files. See 'Storyteller.Writer.Agent.ContextFilter'
--   for the picker model. Real generation no longer applies this layout at
--   all (@context.main@'s own DSL text -- see CONTEXT-DSL.md -- decides
--   what's showable now), so this preview no longer reflects exactly what
--   a real generation call would see; reconciling the two is unbuilt.
data ContextSlot = ContextSlot
  { csLabel  :: T.Text
  , csMode   :: ContextMode
  , csLayout :: ContextLayout
  } deriving (Show, Eq)

-- | One matched file's preview. 'Ambient' slots show the full content, the
--   same as what an agent would actually be handed; 'OnDemand' slots show
--   only a blurb, since the real thing an on-demand slot hands over is a
--   tool the model may or may not call, not injected text.
--
--   'ceBucket' is 'Nothing' for a file no rule in the layout claimed —
--   kept in the list rather than removed so a client can show it shaded-out
--   in place instead of just disappearing, per the design discussion on the
--   Agents tab's context preview. Unclaimed entries never load
--   content/blurb: nothing reads a file that won't be sent anyway. An empty
--   'csLayout' ("nothing configured yet") is the one exception — every file
--   previews as bucket 1, matching how "no layout configured" always
--   falls back to showing everything, rather than every file previewing
--   as unclaimed.
data ContextEntry = ContextEntry
  { cePath    :: FilePath
  , ceContent :: Maybe T.Text
  , ceBlurb   :: Maybe T.Text
  , ceBucket  :: Maybe Int
  } deriving (Show, Eq)

data ContextSlotPreview = ContextSlotPreview
  { cspLabel   :: T.Text
  , cspMode    :: ContextMode
  , cspEntries :: [ContextEntry]
  } deriving (Show, Eq)

-- | Resolve one slot: always list every file the slot could possibly see
--   ("**/*"), then classify each one by 'Storyteller.Writer.Agent.ContextFilter.classifyPath'
--   rather than using the layout to narrow which paths are listed at all.
--   That keeps the same full listing (and thus the same shaded-not-missing
--   behaviour, see 'ContextEntry's 'ceBucket') regardless of how many
--   buckets are in play. Only claimed entries get their content/blurb
--   loaded, per the slot's mode. Same 'FileSystem'\/'FileSystemRead' pair
--   every other plain filesystem read in this codebase requires — not a
--   new capability.
buildSlotPreview
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => ContextSlot
  -> Sem r ContextSlotPreview
buildSlotPreview (ContextSlot label mode layout) = do
  matched <- glob @project "/" "**/*"
  let normalized p = dropWhile (== '/') p
      bucketOf p
        | null layout = Just 1
        | otherwise   = classifyPath layout (normalized p)
  entries <- mapM (loadEntry @project mode . (\p -> (p, bucketOf p))) (List.nub (List.sort matched))
  return (ContextSlotPreview label mode entries)

loadEntry
  :: forall project r
  .  Members '[FileSystemRead project, Fail] r
  => ContextMode -> (FilePath, Maybe Int) -> Sem r ContextEntry
loadEntry _ (path, Nothing) = return (ContextEntry path Nothing Nothing Nothing)
loadEntry mode (path, bucket@(Just _)) = do
  text <- TE.decodeUtf8 <$> readFile @project path
  return $ case mode of
    Ambient  -> ContextEntry path (Just text) Nothing bucket
    OnDemand -> ContextEntry path Nothing (Just (blurb text)) bucket

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
