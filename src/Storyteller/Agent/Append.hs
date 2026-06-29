{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Append agent: split caller-provided text into paragraph atoms and commit
-- each as its own tick. The simplest write path — no LLM involved.
--
-- Richer agents compose this at the end of their pipeline.
module Storyteller.Agent.Append
  ( appendAgent
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem ( FileSystem, FileSystemRead, FileSystemWrite
                        , fileExists, readFile, writeFile )

import Storyteller.Agent.Splitter (Splitter, splitAtoms)
import Storyteller.Git (BranchTag)
import Storyteller.Storage (StoryBranch, StoryStorage, store)
import Storyteller.Types (TickId)

import Prelude hiding (readFile, writeFile)

-- | Split @content@ into paragraph atoms, append each to @path@, and commit
-- each atom as its own tick. Returns the list of created tick IDs.
appendAgent
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[ FileSystem project, FileSystemRead project, FileSystemWrite project
                , StoryBranch branch, StoryStorage, Splitter, Fail ] r )
  => FilePath -> T.Text -> Sem r [TickId]
appendAgent path content = do
  existing <- fileExists @project path >>= \case
    True  -> TE.decodeUtf8 <$> readFile @project path
    False -> return ""
  atoms <- splitAtoms content
  let sep = if T.null existing || T.isSuffixOf "\n\n" existing then ""
            else if T.isSuffixOf "\n" existing then "\n"
            else "\n\n"
      atomSuffixes = case atoms of
        []     -> []
        (a:as) -> (sep <> a) : map ("\n\n" <>) as
      atomContents = tail $ scanl (<>) existing atomSuffixes
  mapM (\(atom, full) -> do
    writeFile @project path (TE.encodeUtf8 full)
    store @branch (T.take 60 atom)) (zip atoms atomContents)
