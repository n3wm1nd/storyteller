{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Append, remove, and rewrite atoms, verbatim. Plain compositions of
-- storage primitives — no LLM, no splitting policy, same category as
-- 'Storyteller.Core.Edit'.
--
-- A position-relative write-then-commit (append onto whatever's at the
-- current filesystem position, then commit that as an 'Atom' tick) and
-- 'Storyteller.Core.Storage.drop' (position-relative removal) are the two
-- moves everything here is built from: 'storeAtom' is the former under a
-- fresh 'withFS'; 'unstoreAtom' is 'drop' at an arbitrary tick with the tail
-- replayed back on top; 'rewriteAtom' is both at once, under a single
-- rewind so the replayed tail lands on top of the edit instead of after it.
-- 'Storyteller.Core.Edit' reuses 'rewriteAtom' and 'unstoreAtom' for its own
-- chain-editing operations (@editAtom@, @commitAtom@'s in-place rewrite)
-- rather than re-deriving the drop-and-replay dance inline each time.
--
-- The write-then-commit pair itself (an 'appendFile' immediately followed
-- by a matching 'storeAs' of an 'Atom') is deliberately *not* factored into
-- its own named function anywhere, including here, even though 'storeAtom',
-- 'rewriteAtom', 'Storyteller.Core.Edit's @emitStandaloneGap@, and its
-- @splitTick@ all write it out by hand. It's only two lines, and its
-- correctness depends entirely on unseen context at the call site (the
-- filesystem position must already be known-clean, matching the tick this
-- is about to become the child of — see 'storeAtom's own comment); wrapping
-- that in an innocuous-looking helper invites calling it from somewhere that
-- doesn't hold, which silently or (since 'Store' now checks) loudly breaks.
-- Writing it out each time keeps the precondition visible in its own
-- context instead of hidden behind a name.
--
-- There is no separate "append many atoms from split text" operation here:
-- that's just 'append', called once per atom produced by
-- 'Storyteller.Common.Splitter.splitAtoms' — an ordinary composition at the
-- call site (@mapM (append \@branch path) =<< splitAtoms content@), not a
-- special case this module needs to know about.
module Storyteller.Core.Append
  ( append
  , appendAtom
  , storeAtom
  , unstoreAtom
  , rewriteAtom
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy
import Polysemy.Fail (Fail)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, appendFile)


import Storyteller.Core.Atom (Atom(..))
import Storyteller.Core.Git (BranchTag)
import Storyteller.Core.Storage (StoryBranch, drop, sneakyAt, storeAs, withFS)
import Storyteller.Core.Types (TickId)

import Prelude hiding (appendFile, drop)

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
storeAtom path content =
  -- 'withFS' is what makes this untouched by the ambient tree: it swaps in a
  -- throwaway copy of HEAD's own snapshot to write into, then discards that
  -- copy and restores the real ambient tree exactly as it was — so the
  -- commit below never sees, and never affects, whatever the caller's live
  -- working tree currently holds. Note this is *not* calling 'appendAtom':
  -- there's no second, ambient-tree write here at all, isolated or not.
  withFS @branch $ do
    appendFile @(BranchTag branch) path (TE.encodeUtf8 content)
    storeAs @branch (Atom path content)

-- | The dual of 'storeAtom': drop @tid@ — an atom tick anywhere in the
-- branch's history, not necessarily HEAD — and replay everything after it
-- back on top, restoring the diff that tick's commit had folded in. Returns
-- the old->new id mapping for the replayed tail, the same shape
-- 'Storyteller.Core.Storage.sneakyAt' returns, for a caller to fold into its
-- own running rebase table before a single broadcast.
unstoreAtom
  :: forall branch r
  .  Members '[StoryBranch branch, Fail] r
  => TickId -> Sem r [(TickId, TickId)]
unstoreAtom tid = snd <$> sneakyAt @branch tid (drop @branch)

-- | Replace tick @tid@ in place with a freshly-appended atom: drop it, then
-- write @content@ onto whatever's left at that position, all under one
-- rewind so the tail replays on top of the edit rather than after it lands
-- somewhere else. Returns the new tick's id and the tail's old->new mapping.
--
-- 'unstoreAtom' followed by a separate 'storeAtom' call cannot substitute
-- for this: 'unstoreAtom's own replay would already have moved head past the
-- tail by the time the second call ran, landing the new content at the end
-- of the chain instead of back in @tid@'s slot.
rewriteAtom
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[ StoryBranch branch, FileSystem project, FileSystemRead project, FileSystemWrite project, Fail ] r )
  => TickId -> FilePath -> T.Text -> Sem r (TickId, [(TickId, TickId)])
rewriteAtom tid path content = sneakyAt @branch tid $ do
  drop @branch
  withFS @branch $ do
    appendFile @project path (TE.encodeUtf8 content)
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
  -- Isolated commit first: HEAD advances, but nothing here has touched the
  -- ambient tree yet — see 'storeAtom'.
  newTid <- storeAtom @branch path content
  -- Then a plain, unconditional write onto whatever the ambient tree
  -- currently holds for @path@ — dirty or clean, this doesn't check, and
  -- doesn't need to: it's a live-view convenience for the caller, entirely
  -- separate from the commit above.
  appendFile @(BranchTag branch) path (TE.encodeUtf8 content)
  return newTid

-- | Ensure text ends with a newline — an appended atom is one text block on
-- disk, and a block should end its line.
ensureTrailingNewline :: T.Text -> T.Text
ensureTrailingNewline t
  | "\n" `T.isSuffixOf` t = t
  | otherwise = t <> "\n"
