-- | Annotation-style tick types and the operations that create them.
--
-- Distinct from chain-editing operations (which restructure the chain
-- itself — delete/move/replace an existing tick in place, see
-- 'Storyteller.Core.StorageMonad'): annotations are just new ticks
-- referencing an existing one — closer in kind to what an agent produces
-- than to a chain-editing primitive. Agents introduce new annotation tick
-- types fairly freely (see 'Storyteller.Common.Types.Note'), so this is
-- where that vocabulary and its constructors collect, rather than under
-- either 'Server.Core.Branch' or 'Server.Core.File' (both need it, neither
-- owns it) or under 'Storyteller.Core.StorageMonad' (it isn't
-- restructuring anything, and 'Note' is app vocabulary, not a generic
-- tick-chain concept). Lives in 'Common' rather than 'Core' for the same
-- reason 'Note' itself does: not foundational, but not specific to any one
-- app either.
module Storyteller.Common.Annotation
  ( addNote
  ) where

import qualified Data.Text as T

import Storyteller.Core.StorageMonad (StorageM, StorageT, followChain, storeAs)
import Storyteller.Core.Types (TickId(..), tickId, tickParent)
import Storyteller.Common.Types (Note(..))

-- | Add an annotation note referencing zero or more existing ticks — zero
--   is valid, a free-floating remark rather than a comment on any specific
--   atom.
addNote :: StorageM m => [TickId] -> T.Text -> StorageT m ()
addNote refs text = do
  ticks <- followChain [] $ \acc t -> (t : acc, tickParent t)
  let known   = map tickId ticks
      missing = filter (`notElem` known) refs
  case missing of
    (bad : _) -> fail $ "ref tick not found: " <> T.unpack (unTickId bad)
    []        -> () <$ storeAs (Note refs text)
