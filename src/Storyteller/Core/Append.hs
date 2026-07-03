{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Append a single atom, verbatim. A plain composition of storage
-- primitives — no LLM, no splitting policy, same category as
-- 'Storyteller.Core.Edit'.
--
-- There is no separate "append many atoms from split text" operation here:
-- that's just this, called once per atom produced by
-- 'Storyteller.Common.Splitter.splitAtoms' — an ordinary composition at the
-- call site (@mapM (append \@branch path) =<< splitAtoms content@), not a
-- special case this module needs to know about.
module Storyteller.Core.Append
  ( append
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, appendFile)

import Storyteller.Core.Atom (Atom(..))
import Storyteller.Core.Git (BranchTag)
import Storyteller.Core.Storage (StoryBranch, storeAs)
import Storyteller.Core.Types (TickId)

import Prelude hiding (appendFile)

-- | Append @content@ to @path@ as a single atom, verbatim, and commit it.
append
  :: forall branch r
  .  Members '[ StoryBranch branch
              , FileSystem      (BranchTag branch)
              , FileSystemRead  (BranchTag branch)
              , FileSystemWrite (BranchTag branch)
              , Fail ] r
  => FilePath -> T.Text -> Sem r TickId
append path content = do
  let content' = ensureTrailingNewline content
  appendFile @(BranchTag branch) path (TE.encodeUtf8 content')
  storeAs @branch (Atom path content')

-- | Ensure text ends with a newline — an appended atom is one text block on
-- disk, and a block should end its line.
ensureTrailingNewline :: T.Text -> T.Text
ensureTrailingNewline t
  | "\n" `T.isSuffixOf` t = t
  | otherwise = t <> "\n"
