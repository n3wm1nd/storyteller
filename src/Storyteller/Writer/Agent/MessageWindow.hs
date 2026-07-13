-- | Injecting a message (or a few) at a bounded depth inside an existing
-- message list, for LLM prompt-cache stability -- no dependency on
-- 'Storage.Tick' or anything else domain-specific; this only ever looks at
-- the @['UniversalLLM.Message' m]@ shape itself.
--
-- Extracted out of 'Storyteller.Writer.Agent.Write' (where the splice
-- message -- pinned context plus a character's journal excerpt -- needs to
-- sit somewhere inside a chapter's reconstructed conversation) because the
-- underlying problem is general: any caller building a growing @[Message]@
-- history out of persisted turns, that also needs to splice in something
-- recomputed fresh each call, has the same tension and can reuse this
-- directly rather than re-deriving it.
module Storyteller.Writer.Agent.MessageWindow
  ( injectAtWindow
  , windowBoundary
  ) where

import UniversalLLM (Message)

-- | Inject @toInsert@ into @history@ at a depth between @lo@ and @hi@
--   "turns" back from the end -- inclusive, where @isTurnStart@ marks which
--   messages begin a new turn (e.g. @\\case { UserText _ -> True; _ ->
--   False }@ for a plain alternating prompt\/reply history). @toInsert@
--   itself is never split; it lands as a contiguous block. Returns
--   @history@ unchanged (no split, no injection) when @toInsert@ is empty
--   -- there's nowhere for nothing to "sit", so no reason to pay for the
--   scan.
--
--   The depth is *not* recomputed as "always exactly @lo@ turns back from
--   the end" (the @lo == hi@ degenerate case still behaves that way) --
--   that moves the injection point by one turn on every single call, which
--   means the message immediately before it is a different message every
--   time, which means nothing from the injection point onward could ever
--   be served from a prefix cache. Instead the boundary only advances once
--   the tail would exceed @hi@ turns, and when it does, it jumps forward by
--   exactly @hi - lo + 1@ turns, landing the tail back at @lo@, not @0@ --
--   see 'windowBoundary' for the arithmetic. That means the boundary (and
--   everything up to and including the injected block) is byte-for-byte
--   identical across a whole @hi - lo + 1@-turn stretch: for those turns,
--   the result only ever *appends* to what a previous call already
--   produced, which is exactly the shape a provider's prefix cache can
--   serve for free. One turn in every @hi - lo + 1@ pays a reset; the rest
--   are free rides.
injectAtWindow
  :: (Message m -> Bool)  -- ^ does this message start a new turn?
  -> Int                   -- ^ lo: minimum turns kept after the injection
  -> Int                   -- ^ hi: maximum turns kept after the injection
  -> [Message m]           -- ^ what to inject, as one contiguous block
  -> [Message m]           -- ^ the history to inject into
  -> [Message m]
injectAtWindow _ _ _ [] history = history
injectAtWindow isTurnStart lo hi toInsert history
  | boundary == 0 = toInsert ++ history
  | otherwise     = before ++ toInsert ++ after
  where
    turnIdxs = [ i | (i, m) <- zip [0 :: Int ..] history, isTurnStart m ]
    total    = length turnIdxs
    boundary = windowBoundary lo hi total
    (before, after) = splitAt (turnIdxs !! boundary) history

-- | The pure arithmetic 'injectAtWindow' turns into a split index: how many
--   turns sit before the injection point, given @total@ turns exist and the
--   tail after it must hold between @lo@ and @hi@ turns. Produces a step
--   function of @total@: flat at @0@ while @total <= lo@, then flat across
--   each @period = hi - lo + 1@-turn stretch after that, jumping by
--   @period@ at each reset. @lo == hi@ collapses @period@ to @1@, so the
--   boundary moves on every turn -- the least favourable point on this same
--   spectrum, not a different mechanism.
windowBoundary :: Int -> Int -> Int -> Int
windowBoundary lo hi total
  | total <= lo = 0
  | otherwise   = total - (((total - lo) `mod` period) + lo)
  where period = hi - lo + 1
