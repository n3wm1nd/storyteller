-- | Introduce a new, empty file into the tree as its own tick — a plain
-- composition of storage primitives. Whatever content follows (a
-- chat.append, an agent write) lands as ordinary
-- 'Storyteller.Core.Atom.Atom' ticks afterward; this only records the
-- path's introduction.
module Storyteller.Core.Create
  ( createFile
  ) where

import qualified Data.ByteString as BS

import Storyteller.Core.Created (Created(..))
import Storyteller.Core.StorageMonad (StorageM, StorageT, writeFileS, storeAs)
import Storyteller.Core.Types (TickId)

-- | Write @path@ as an empty file and commit that as a 'Created' tick.
createFile :: StorageM m => FilePath -> StorageT m TickId
createFile path = do
  writeFileS path BS.empty
  storeAs (Created path)
