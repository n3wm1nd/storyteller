{-# LANGUAGE OverloadedStrings #-}

-- | Introduce a new, empty file into the tree as its own tick — a plain
-- composition of storage primitives, no dedicated tick kind of its own.
-- Whatever content follows (a chat.append, an agent write) lands as
-- ordinary 'Storyteller.Core.Atom.Atom' ticks afterward; this tick just
-- records the path's introduction, as an atom with no content of its own
-- -- not a distinct tick kind, so 'Storage.Core.drop's existing 'Atom'
-- handling recovers its diff for free, and the introduction counts as a
-- real content-affecting event, same as any other atom.
--
-- Its counterpart, whole-file deletion, has no wrapper here: it's just
-- 'Storage.Ops.deleteFile' directly (see 'Server.Core.File.deleteFile') --
-- a same-named wrapper at this layer would add nothing 'Storage.Ops'
-- doesn't already say.
module Storyteller.Core.Create
  ( createFile
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
