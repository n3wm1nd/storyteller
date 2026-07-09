{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Alternate-generation ("swipe") operations: swap an atom's own content
-- for another candidate, keeping whatever it held before around as a new
-- alternate rather than discarding it — built entirely from
-- "Storage.Core"/"Storage.Ops"/"Storage.Tick"'s existing primitives, no
-- new chain-editing machinery needed there.
--
-- Every alternate for a given atom is a 'Swipe' tick (see
-- "Storyteller.Common.Types") referencing it, and — by construction, see
-- 'swapAtomContent' — always inserted immediately after the atom's own
-- position. That contiguous run right after the atom is its carousel:
-- 'cycleSwipe' rotates through it one step at a time, popping whichever
-- alternate is closest to head into the atom and pushing what the atom
-- held before back onto the near-atom end of the run. N alternates take
-- exactly N cycles to return to the original content — this is a ring,
-- not a stack.
module Storyteller.Common.Swipe
  ( swapAtomContent
  , pushSwipe
  , cycleSwipe
  ) where

import Data.Text (Text)

import Storage.Core (StoreM, StoreT, ObjectHash(..), at, follow, resolveId)
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Core.Types (TickId(..), Tick(..), fromTick)
import Storyteller.Core.Atom (Atom(..))
import Storyteller.Common.Types (Swipe(..))

-- | Replace @tid@'s (an atom's) own content with @newContent@, keeping
--   whatever it held before as a new alternate — a 'Swipe' inserted
--   immediately after the atom's new position, ahead of anything already
--   in its carousel. The shared core both 'pushSwipe' and 'cycleSwipe'
--   build on.
swapAtomContent :: StoreM m => ObjectHash -> Text -> StoreT m ObjectHash
swapAtomContent tid newContent = do
  old <- Tick.readTypesTick tid
  oldContent <- case fromTick @Atom old of
    Just (Atom _ content) -> return content
    Nothing                -> fail "swapAtomContent: not an atom"
  newTid <- Ops.editAtomAt tid newContent
  _ <- at newTid (Tick.storeAs (Swipe (TickId (unObjectHash newTid)) oldContent))
  return newTid

-- | Land freshly generated content on @tid@ (an atom), keeping its
--   previous content as a cycle-able alternate — the regenerate-and-keep-
--   history call site.
pushSwipe :: StoreM m => ObjectHash -> Text -> StoreT m ObjectHash
pushSwipe = swapAtomContent

-- | Rotate @tid@'s (an atom's) own carousel of alternates forward one
--   step. Fails if it has none. @tid@ is resolved first (see
--   'Storage.Core.resolveId') -- a caller composing this with an earlier
--   edit in the same scope may be holding an id that edit has since
--   replaced.
cycleSwipe :: StoreM m => ObjectHash -> StoreT m ObjectHash
cycleSwipe tid0 = do
  tid <- resolveId tid0
  chain <- fullChain
  case swipeRunAfter tid chain of
    [] -> fail "cycleSwipe: no alternates"
    run -> do
      let (popHash, poppedContent) = last run
      Ops.deleteTick popHash
      swapAtomContent tid poppedContent

-- | The whole chain, oldest first, decoded via the typed layer — same
--   'follow' shape 'Storage.Ops.contentChain' already uses (prepending
--   each newly-walked, older tick onto the accumulator already leaves it
--   oldest-first; no reversal needed).
fullChain :: StoreM m => StoreT m [(ObjectHash, Tick)]
fullChain = do
  hashes <- follow [] (\acc h _t -> (h : acc, True))
  mapM (\h -> (,) h <$> Tick.readTypesTick h) hashes

-- | The contiguous run of 'Swipe' ticks immediately following @tid@ in
--   @chain@, oldest-first (nearest-the-atom first, closest-to-head last)
--   — stops at the first tick that isn't one of @tid@'s own alternates.
swipeRunAfter :: ObjectHash -> [(ObjectHash, Tick)] -> [(ObjectHash, Text)]
swipeRunAfter tid chain = case break ((== tid) . fst) chain of
  (_, _ : rest) -> takeMatching rest
  (_, [])       -> []
  where
    target = TickId (unObjectHash tid)
    takeMatching ((h, t) : rest) = case fromTick @Swipe t of
      Just (Swipe of_ content) | of_ == target -> (h, content) : takeMatching rest
      _                                        -> []
    takeMatching [] = []
