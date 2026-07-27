{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The story's known cast — every @character\/*@ branch, with its sheet.
--
-- Project-global, not scoped to any one open branch: answering this means
-- enumerating branches ('Storyteller.Core.Storage.StoryStorage') and
-- entering each one ('Storyteller.Core.Branch.Branches'), so a caller
-- holds both.
module Storyteller.Writer.Cast
  ( CastMember(..)
  , knownCast
  ) where

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Polysemy
import Polysemy.Fail (Fail)

import Runix.FileSystem (FileSystemRead(..))
import Storyteller.Core.Branch (Branches, withBranch)
import Storyteller.Core.Git (BranchTag(..), runStoryFSRead)
import Storyteller.Core.Storage (StoryStorage, listBranches)
import Storyteller.Core.Types (Branch(..), BranchName(..))
import Storyteller.Writer.Branches (BranchKind(..), classifyBranch)

-- | One member of the story's known cast: its branch identity and
--   @sheet.md@ verbatim (empty if the branch has none yet — a character
--   branch created but not fleshed out is still a legitimate cast
--   member). Same \"hand the raw sheet over, let the caller decide what to
--   do with it\" shape 'Server.Writer.Character.characterState' already
--   gives the sidebar. No display name here — that's
--   'Storyteller.Writer.Branches.branchDisplayName' applied to 'cmBranch',
--   a presentation concern.
data CastMember = CastMember
  { cmBranch :: BranchName
  , cmSheet  :: Text
  } deriving (Show, Eq)

-- | Every character branch and what its sheet says.
--
--   __The per-member read is not the cheapest possible one, on purpose.__
--   'Storyteller.Core.Git.runStoryFSRead' materializes the branch's whole
--   readable-content tree on entry — references only, but the atom-tracked
--   filter behind it is a chain walk, so the cost grows with that
--   character's history and is paid once per cast member. A positioned
--   single-path read ('Storage.Core.readPathAt') would be O(path depth)
--   and history-independent.
--
--   The filesystem effects are the primary way file data is reached here,
--   and that is worth more than the difference. A function written against
--   'Runix.FileSystem.FileSystemRead' runs against a branch, a snapshot, a
--   real directory or a test filesystem without knowing which; a function
--   written against a positioned read primitive runs against exactly one
--   backend and drags that backend into its own signature. Paying a
--   bounded, references-only overhead to keep every reader portable is the
--   trade this codebase makes deliberately — see 'Storyteller.Core.Snapshot'
--   for the same choice.
--
--   What /would/ be worth revisiting is not the per-read cost but how many
--   times a scope gets opened at all: this opens one per cast member, and
--   callers above it sometimes re-enter branches an outer scope already
--   had. That is a structuring question about scope lifetimes, not an
--   argument for reaching past the filesystem effects.
knownCast
  :: forall castBranch r
  .  Members '[StoryStorage, Branches, Fail] r
  => Sem r [CastMember]
knownCast = do
  branches <- listBranches
  mapM toCastMember [ b | b <- branches, classifyBranch (unBranchName (branchName b)) == Character ]
  where
    toCastMember b = do
      let name = branchName b
      -- The raw 'ReadFile' constructor rather than
      -- 'Runix.FileSystem.readFile', because a character branch with no
      -- sheet yet is a legitimate cast member (see 'CastMember') — the
      -- miss is an empty answer here, not a 'Fail'.
      sheet <- withBranch @castBranch name $ runStoryFSRead @(BranchTag castBranch) @castBranch (BranchTag name) $
        either (const "") TE.decodeUtf8 <$> send @(FileSystemRead (BranchTag castBranch)) (ReadFile "sheet.md")
      pure CastMember { cmBranch = name, cmSheet = sheet }
