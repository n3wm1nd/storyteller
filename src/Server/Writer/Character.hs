{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Composition for the @\/character\/{charBranch}@ connection: the
-- sidebar-facing view of a character branch. Writer-specific in the same
-- way 'Server.Writer.Branch' is — it knows the @character\/{id}@ naming and
-- @sheet.md@ file conventions documented in WRITER.md, which
-- 'Server.Core.Branch' has no business knowing about.
--
-- Deliberately grows by accretion: today this is just display name + sheet
-- content. Adding a new sidebar field (mood, status, ...) means adding a
-- field here and to 'Server.Writer.Character.Protocol.CharacterEvent', not
-- inventing a new connection.
module Server.Writer.Character
  ( CharacterState(..)
  , characterState
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy (Members, Sem)
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, fileExists, readFile)

import Storyteller.Writer.Branches (branchDisplayName)

import Prelude hiding (readFile)

data CharacterState = CharacterState
  { charName      :: T.Text
  , charSheet     :: Maybe T.Text
  , charHasAvatar :: Bool
  } deriving (Show, Eq)

-- | Display name is the branch name with the @character\/@ prefix
--   stripped, when present — collected-and-augmented, not processed: no
--   summarization, just what's directly readable off the branch.
--   'charHasAvatar' is a plain existence check on @avatar.png@ (deposited
--   by a SillyTavern character card import, see
--   'Server.Writer.Branch.importCharacterCard') — an existence flag, not
--   the image bytes themselves: the client already has a plain @GET
--   \/branch\/{name}\/avatar.png@ available for the bytes (same route any
--   other branch file uses), so there's no reason to duplicate binary
--   content over this JSON push the way 'charSheet' duplicates text.
--   Generic over @project@ -- two 'fileExists' and a 'readFile' is the
--   whole operation, so this asks for a filesystem and nothing else. It
--   used to say 'Server.Core.Branch.BranchOpen', which bundles
--   'Storyteller.Core.Branch.BranchOp', 'StoryStorage' and
--   'Runix.FileSystem.FileSystemWrite' along with the read effects -- i.e.
--   it declared a writable, git-backed, chain-editing branch scope in
--   order to read two files. Nothing about "what does this character's
--   sheet say" needs a tick chain, a content-addressed object store, or
--   the ability to write, and now nothing about the type claims otherwise:
--   any 'Runix.FileSystem' interpreter at all can serve it -- a live
--   branch scope ('Storyteller.Core.Git.runStoryFSGit'), a committed
--   snapshot ('Storyteller.Core.Snapshot.runSnapshotFS'), a plain
--   directory, a filter stacked on any of those.
characterState
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => T.Text -> Sem r CharacterState
characterState branch = do
  let name = branchDisplayName branch
  sheet <- fileExists @project "sheet.md" >>= \case
    False -> return Nothing
    True  -> Just . TE.decodeUtf8 <$> readFile @project "sheet.md"
  hasAvatar <- fileExists @project "avatar.png"
  return (CharacterState name sheet hasAvatar)
