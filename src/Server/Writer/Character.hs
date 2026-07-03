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
import Polysemy (Sem)
import Runix.FileSystem (fileExists, readFile)

import Server.Core.Branch (Main, BranchOpen)
import Storyteller.Core.Git (BranchTag)

import Prelude hiding (readFile)

data CharacterState = CharacterState
  { charName  :: T.Text
  , charSheet :: Maybe T.Text
  } deriving (Show, Eq)

-- | Display name is the branch name with the @character\/@ prefix
--   stripped, when present — collected-and-augmented, not processed: no
--   summarization, just what's directly readable off the branch.
characterState :: BranchOpen r => T.Text -> Sem r CharacterState
characterState branch = do
  let name = maybe branch id (T.stripPrefix "character/" branch)
  sheet <- fileExists @(BranchTag Main) "sheet.md" >>= \case
    False -> return Nothing
    True  -> Just . TE.decodeUtf8 <$> readFile @(BranchTag Main) "sheet.md"
  return (CharacterState name sheet)
