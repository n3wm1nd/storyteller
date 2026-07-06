{-# LANGUAGE OverloadedStrings #-}

-- | Introduce a new, empty file into the tree as its own tick — a plain
-- composition of storage primitives. Whatever content follows (a
-- chat.append, an agent write) lands as ordinary
-- 'Storyteller.Core.Atom.Atom' ticks afterward; this tick just records the
-- path's introduction, as an atom with no content of its own -- not a
-- distinct tick kind, so 'Storyteller.Core.StorageMonad.popTick's existing
-- 'Atom' handling recovers its diff for free, and the introduction counts
-- as a real content-affecting event, same as any other atom.
module Storyteller.Core.Create
  ( createFile
  ) where

import qualified Data.ByteString as BS

import Storyteller.Core.Atom (Atom(..))
import Storyteller.Core.StorageMonad (StorageM, StorageT, writeFileS, addAtom)
import Storyteller.Core.Types (TickId, toDraft)

-- | Write @path@ as an empty file and commit that as an empty atom tick.
--   Uses 'addAtom' directly, not 'Storyteller.Core.StorageMonad.storeAtom':
--   the empty write just above already landed on the ambient tree, and a
--   caller doing further ambient 'Runix.FileSystem' operations
--   immediately afterward (in the same branch scope, a separate dispatch)
--   needs to see it there -- 'storeAtom's 'withFS' isolation would revert
--   the ambient tree to before this call once done, making the
--   just-created file disappear from it.
createFile :: StorageM m => FilePath -> StorageT m TickId
createFile path = do
  writeFileS path BS.empty
  addAtom path "" (toDraft (Atom path ""))
