{-# LANGUAGE OverloadedStrings #-}

-- | Introduce or remove a whole file from the tree, at the tick level —
-- a plain composition of storage primitives, no dedicated tick kind for
-- either direction.
--
-- 'createFile' records the path's introduction as an atom with no content
-- of its own -- not a distinct tick kind, so 'Storage.Core.drop's existing
-- 'Atom' handling recovers its diff for free, and the introduction counts
-- as a real content-affecting event, same as any other atom. Whatever
-- content follows (a chat.append, an agent write) lands as ordinary
-- 'Storyteller.Core.Atom.Atom' ticks afterward.
--
-- 'deleteFile' is a forward event, not a rebase: it commits one more,
-- ordinary 'Storage.Core.Atom' tick -- via 'Storage.Ops.removeFile' -- that
-- happens to remove @path@ from the tree instead of adding to it, rather
-- than rewriting history to make it look like the file was never there.
-- Every earlier atom on @path@ stays exactly where it was, so a rebase to
-- any tick before the deletion still sees the file as it was then -- undo
-- is the same generic chain-rewind (walk head back) any other write here
-- already gets, no bespoke mechanism needed. See 'Storage.Core.store's own
-- Haddock for why a rebase-based delete (physically dropping those earlier
-- ticks, the way this module's chain-editing primitives like
-- 'Storage.Ops.deleteTick' do for correcting a single misplaced atom) would
-- have been the wrong shape here.
module Storyteller.Core.Create
  ( createFile
  , deleteFile
  ) where

import Storage.Core (StoreM, StoreT, ObjectHash)
import qualified Storage.Ops as Ops

-- | Write @path@ as an empty file and commit that as an empty atom tick.
--   'Storage.Ops.addAtom' both commits and lands the same content in the
--   ambient tree, so a caller doing further ambient 'Runix.FileSystem'
--   operations immediately afterward (in the same branch scope, a
--   separate dispatch) sees the just-created file there.
createFile :: StoreM m => FilePath -> StoreT m ObjectHash
createFile path = Ops.addAtom path ""

-- | Commit @path@'s deletion, returning the new tick's own id -- see the
--   module Haddock and 'Storage.Ops.removeFile' for why this is a forward
--   tick, not a rebase. Removing that returned id later (the ordinary
--   single-tick rebase any other tick already gets, e.g.
--   'Storage.Ops.deleteTick') undoes the deletion.
deleteFile :: StoreM m => FilePath -> StoreT m ObjectHash
deleteFile path = Ops.removeFile path
