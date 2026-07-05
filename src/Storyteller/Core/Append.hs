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
  , appendAtom
  , storeAtom
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, appendFile)

import Storyteller.Core.Atom (Atom(..))
import Storyteller.Core.Git (BranchTag)
import Storyteller.Core.Storage (StoryBranch, storeAs, withFS)
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
append path content = appendAtom @branch path (ensureTrailingNewline content)

-- | Commit @content@ as a new 'Atom' tick appended to @path@'s own HEAD-
-- committed value — entirely independent of whatever the live working
-- tree currently holds, for @path@ or any other file. Never reads or
-- writes the ambient tree: built under 'withFS', which loads a throwaway
-- copy of HEAD's own committed snapshot to append onto and commit, then
-- restores the ambient exactly as it was. Only the branch's tracked
-- position (and the real git ref) advances to the new commit.
--
-- This is the one primitive 'Storyteller.Core.Edit.commitFile'-style
-- reconciliation actually needs: it can record an atom's content as
-- history without ever touching the file it's reconciling. 'appendAtom'
-- is this plus a plain write to the live working tree, for a caller that
-- also wants their own view to reflect the new atom immediately.
storeAtom
  :: forall branch r
  .  Members '[ StoryBranch branch
              , FileSystem      (BranchTag branch)
              , FileSystemRead  (BranchTag branch)
              , FileSystemWrite (BranchTag branch)
              , Fail ] r
  => FilePath -> T.Text -> Sem r TickId
storeAtom path content = withFS @branch $ do
  appendFile @(BranchTag branch) path (TE.encodeUtf8 content)
  storeAs @branch (Atom path content)

-- | Append @content@ to @path@ and commit it as a real 'Atom' tick, with no
-- newline normalization — the primitive 'append' builds on, and the one
-- call this project should use anywhere a file write needs to land on the
-- chain as a real atom. An atom's own content lives verbatim in its commit
-- message (see 'Storyteller.Core.Atom.contentFor'), so pairing a raw
-- filesystem write with anything other than 'storeAs' of the same content —
-- a plain 'Storyteller.Core.Storage.store', a hand-built 'TickData' — leaves
-- that content invisible to every reader that decodes it from the message
-- (fileTicks, popTick, buildAtomHistory, trackBranch) without the type
-- system ever flagging the mismatch.
--
-- 'storeAtom', plus a plain, ordinary append of the same content onto the
-- live working tree, unconditionally — this never checks whether @path@
-- already had some other pending, uncommitted edit sitting in it. If it
-- did, the tree afterward just holds more than this one commit's message
-- claims; that's not a new problem, it's the ordinary "working tree has a
-- pending diff against HEAD" situation 'Storyteller.Core.Edit.commitFiles'
-- already exists to reconcile. Refusing instead would trade a perfectly
-- reconcilable situation for a hard failure — worse, not safer.
appendAtom
  :: forall branch r
  .  Members '[ StoryBranch branch
              , FileSystem      (BranchTag branch)
              , FileSystemRead  (BranchTag branch)
              , FileSystemWrite (BranchTag branch)
              , Fail ] r
  => FilePath -> T.Text -> Sem r TickId
appendAtom path content = do
  newTid <- storeAtom @branch path content
  appendFile @(BranchTag branch) path (TE.encodeUtf8 content)
  return newTid

-- | Ensure text ends with a newline — an appended atom is one text block on
-- disk, and a block should end its line.
ensureTrailingNewline :: T.Text -> T.Text
ensureTrailingNewline t
  | "\n" `T.isSuffixOf` t = t
  | otherwise = t <> "\n"
