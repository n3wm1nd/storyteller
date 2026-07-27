{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Run an arbitrary action "as if" an earlier tick were head — a thin
--   Polysemy front for 'Storyteller.Core.Git.atGeneric', the same
--   rebase-marker mechanism every other "run this command as if @tid@ were
--   HEAD" call site (@Server.Writer.File.Dispatch@,
--   @Storyteller.Writer.Agent.Summarizer@, ...) already uses directly.
--
--   This module exists for exactly one kind of caller: a one-off, "insert
--   one thing at a specific historical position" operation — not a general
--   replacement for composing several chain edits together, and not
--   something a caller should reach for when a plain 'Storyteller.Core.
--   Branch.BranchOp'\/'Storage.Core.StoreT' computation (dispatched once,
--   no 'interpretH' needed) already says what's wanted. See
--   'Storyteller.Core.Git.atGeneric's own Haddock for why a higher-order
--   effect is unavoidable here specifically: the inner action can be
--   arbitrary 'Sem' code (an LLM call, another dispatch), not a closed-form
--   'Storage.Core.StoreT' computation the way every other chain-editing
--   effect in this codebase gets away with plain first-order 'interpret'.
--
--   Two ways to name the position: 'At' takes a 'TickId' directly (the
--   common case, when the caller already has one — a beat sheet's own
--   chapter tick, a specific atom just read off 'Storage.Tick.FileTick').
--   'AtAtomText' takes a file path and a quoted span instead, for a caller
--   that only knows *what the position looks like*, not its id — finds the
--   one atom on that file whose text contains the span, exactly once, and
--   runs there; fails (not guesses) if the match is missing or ambiguous,
--   the same discipline 'Storyteller.Writer.Agent.ReplaceTool.replaceOnce'
--   already applies to a model-supplied span within one atom, extended
--   here to "which atom" across a whole file.
module Storyteller.Core.Timetravel
  ( Timetravel(..)
  , at
  , atAtomText
  , runTimetravel
  ) where

import Data.Kind (Type)
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail (Fail)

import Storage.Tick (FileTick(..))
import qualified Storage.Tick as Tick
import Storage.Tick (atomsMatchingText)

import Storyteller.Core.Branch (BranchOp)
import Storyteller.Core.Git (atGeneric, runStorage)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (TickId(..))

-- | @branch@-scoped, same convention as 'Storyteller.Core.Branch.BranchOp'
--   and @Runix.FileSystem@: "run this at a position" is meaningless
--   without saying which branch's chain the position is in.
data Timetravel (branch :: k) (m :: Type -> Type) a where
  At         :: TickId -> m a -> Timetravel branch m a
  AtAtomText :: FilePath -> T.Text -> m a -> Timetravel branch m a

-- | Run @action@ as if @tid@ were this branch's head.
at :: forall branch r a. Member (Timetravel branch) r => TickId -> Sem r a -> Sem r a
at tid action = send @(Timetravel branch) (At tid action)

-- | Run @action@ as if the one atom on @path@ whose text contains
--   @atText@ were this branch's head — see the module Haddock.
atAtomText :: forall branch r a. Member (Timetravel branch) r => FilePath -> T.Text -> Sem r a -> Sem r a
atAtomText path atText action = send @(Timetravel branch) (AtAtomText path atText action)

-- | 'At' is a direct 'interpretH' front for
--   'Storyteller.Core.Git.atGeneric' — 'runTSimple' hands the inner action
--   over as an ordinary 'Sem' computation (it interleaves whatever effects
--   are already live in @r@, nothing 'Timetravel'-specific), and
--   'atGeneric' does the actual descend\/run\/replay.
--
--   'AtAtomText' resolves its span against @path@'s current atoms via
--   'Storage.Tick.atomsMatchingText' (fetched once via
--   'Storage.Tick.fileTicksOf', one 'BranchOp' dispatch) before delegating
--   to the same 'atGeneric' call 'At' uses — matching, not positioning, is
--   the only thing it adds, and this caller's own tolerance is exactly one
--   match: anything else fails rather than guessing.
runTimetravel
  :: forall branch r a
  .  Members '[BranchOp branch, StoryStorage, Fail] r
  => Sem (Timetravel branch ': r) a -> Sem r a
runTimetravel = interpretH $ \case
  At tid inner ->
    atGeneric @branch tid (runTSimple inner)

  AtAtomText path atText inner -> do
    ticks <- raise (runStorage @branch (Tick.fileTicksOf path))
    case atomsMatchingText atText ticks of
      [matched] -> atGeneric @branch (TickId (ftTickId matched)) (runTSimple inner)
      []  -> raise $ fail $ "atAtomText: \"" <> T.unpack atText <> "\" didn't match any atom in " <> path
      _   -> raise $ fail $ "atAtomText: \"" <> T.unpack atText <> "\" matched more than one atom in " <> path
