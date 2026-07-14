{-# LANGUAGE OverloadedStrings #-}

-- | The single place a branch *name* (as opposed to a file path within one
-- — see "Storyteller.Writer.Library" for that) is classified by convention
-- — see WRITER.md's "Branch naming" for the authoritative rule; keep the
-- two in sync if it changes. Mirrored (not shared) by
-- @frontend/src/lib/branches.ts@, the one UI spot (the plain "Branches"
-- sidebar tab) that needs to classify a raw branch-name list on its own —
-- see that module's header.
--
-- Before this existed, the one convention actually enforced anywhere
-- (@"character/"@) was duplicated verbatim across half a dozen call sites
-- in four different modules (@Server.Writer.Character@,
-- @Server.Writer.File@, @Server.Writer.Session.Connection@,
-- @Server.Writer.Session.Dispatch@) with no shared definition — exactly the
-- "independently reimplemented, silently drifting" failure mode WRITER.md's
-- own preamble warns about. This module is that shared definition.
module Storyteller.Writer.Branches
  ( BranchKind(..)
  , classifyBranch
  , branchDisplayName
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Storyteller.Core.Prompt (promptsBranchName)
import Storyteller.Core.Types (BranchName(..))

-- | What convention (if any) a branch name matches. There is no @Other@
--   case the way "Storyteller.Writer.Library"'s @OtherFile@ is one: a
--   branch matching neither 'Character' nor 'Prompts' is a 'Story' branch
--   by default, not an unrecognized one — @story/{storythread}@ is an
--   optional, purely cosmetic prefix (stripped by 'branchDisplayName' when
--   present), never required, since a plain branch name with no prefix at
--   all (@"master"@, say) already works as a story branch everywhere in
--   this codebase today.
data BranchKind
  = Character  -- ^ @character/{characterid}@ — see WRITER.md's "Branch naming".
  | Prompts    -- ^ The one well-known @"prompts"@ branch ('Storyteller.Core.Prompt.promptsBranchName').
  | Story      -- ^ Everything else — the default, not a fallback for "unrecognized".
  deriving (Show, Eq)

-- | Classify a branch name. See the module Haddock for the algorithm; this
--   is the one place it's implemented (mirrored, not shared, in
--   @frontend/src/lib/branches.ts@ — see WRITER.md).
classifyBranch :: Text -> BranchKind
classifyBranch name
  | name == unBranchName promptsBranchName = Prompts
  | "character/" `T.isPrefixOf` name       = Character
  | otherwise                               = Story

-- | A branch's own display name — its known prefix stripped, when present.
--   Same "server hands over raw text, client decides further presentation"
--   contract as everywhere else in this codebase (a character's *real*
--   display name still comes from @sheet.md@'s own H1 line, same as
--   WRITER.md documents — this is only the branch-id-level fallback every
--   caller already needed).
branchDisplayName :: Text -> Text
branchDisplayName name
  | Just rest <- T.stripPrefix "character/" name = rest
  | Just rest <- T.stripPrefix "story/" name      = rest
  | otherwise                                     = name
