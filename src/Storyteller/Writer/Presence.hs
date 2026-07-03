{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The operation that creates 'Storyteller.Writer.Types.Presence' ticks —
-- same relationship to 'Storyteller.Writer.Types' as
-- 'Storyteller.Common.Annotation' has to 'Storyteller.Common.Types'.
module Storyteller.Writer.Presence
  ( recordPresence
  ) where

import qualified Data.Text as T
import Polysemy
import Polysemy.Fail

import Storyteller.Core.Storage (StoryBranch, StoryStorage, getBranch, storeAs)
import Storyteller.Core.Types (BranchName(..), TickId)
import Storyteller.Writer.Types (Presence(..), PresenceEvent)

-- | Record a character entering or leaving the scene on @branch@'s chain.
--   Fails if the character branch doesn't exist — this is a reference to
--   another branch, not a tick within this one, so there's no chain-walk
--   integrity check to lean on; this is the one check available.
recordPresence
  :: forall branch r
  .  Members '[StoryBranch branch, StoryStorage, Fail] r
  => BranchName -> PresenceEvent -> Sem r TickId
recordPresence character event =
  getBranch character >>= \case
    Nothing -> fail ("character branch not found: " <> T.unpack (unBranchName character))
    Just _  -> storeAs @branch (Presence character event)
