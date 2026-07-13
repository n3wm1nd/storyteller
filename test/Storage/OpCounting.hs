{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A 'MonadStore' interceptor: wraps any existing store and counts the
--   physical operations ('readObject'\/'readCommit' vs.
--   'writeObject'\/'writeCommit') an action performs against it, without
--   touching the underlying store's own implementation at all. Lets a
--   test assert "no ticks were replaced" (@ocWrites == 0@) or "this cost
--   didn't grow with the graph" (the same 'OpCounts' whether run over a
--   tiny or a huge amount of unrelated history) directly, instead of
--   inferring either from wall-clock time.
--
--   'resolveHash'\/'recordRemap' aren't counted: they're an in-memory
--   table lookup against the store's own remap table in any real backend
--   too, not I\/O -- the same reason 'Storage.Core''s own Haddock gives
--   for why 'resolveId' is documented as O(1).
module Storage.OpCounting
  ( OpCounts(..)
  , Counting
  , measureOps
  ) where

import Control.Monad.State.Strict

import Storage.Core

data OpCounts = OpCounts
  { ocReads  :: Int
  , ocWrites :: Int
  } deriving (Show, Eq)

newtype Counting m a = Counting (StateT OpCounts m a)
  deriving (Functor, Applicative, Monad, MonadState OpCounts)

instance MonadFail m => MonadFail (Counting m) where
  fail = Counting . lift . fail

instance MonadStore m => MonadStore (Counting m) where
  readObject h = do
    modify (\c -> c { ocReads = ocReads c + 1 })
    Counting (lift (readObject h))
  writeObject o = do
    modify (\c -> c { ocWrites = ocWrites c + 1 })
    Counting (lift (writeObject o))
  readCommit h = do
    modify (\c -> c { ocReads = ocReads c + 1 })
    Counting (lift (readCommit h))
  writeCommit cd = do
    modify (\c -> c { ocWrites = ocWrites c + 1 })
    Counting (lift (writeCommit cd))
  resolveHash h    = Counting (lift (resolveHash h))
  recordRemap o n  = Counting (lift (recordRemap o n))

-- | Run @setup@ against the underlying store, uninstrumented -- its own
--   cost is deliberately excluded, since it exists only to build up
--   whatever history a test wants in place *before* the action actually
--   being measured -- then run @measure@ from wherever @setup@ left head,
--   over the same store, wrapped in 'Counting'. Both phases share the one
--   underlying store (so @measure@ can see everything @setup@ wrote),
--   only @measure@'s own operations are counted.
measureOps
  :: StoreM m
  => ObjectHash
  -> StoreT m a
  -> StoreT (Counting m) b
  -> m (b, OpCounts)
measureOps root setup measure = do
  (_, scope)      <- runStoreT root setup
  let Counting counted = runStoreTFrom scope measure
  ((b, _), counts) <- runStateT counted (OpCounts 0 0)
  return (b, counts)
