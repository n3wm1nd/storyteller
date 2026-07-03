{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Annotation-style tick types and the operations that create them.
--
-- Distinct from "Storyteller.Core.Edit": edit operations restructure the chain
-- itself (delete/move/replace an existing tick in place). Annotations are
-- just new ticks referencing an existing one — closer in kind to what an
-- agent produces than to a chain-editing primitive. Agents introduce new
-- annotation tick types fairly freely (see 'Storyteller.Core.Types.Note'), so
-- this is where that vocabulary and its constructors collect, rather than
-- under either 'Server.Core.Branch' or 'Server.Core.File' (both need it,
-- neither owns it) or under 'Storyteller.Core.Edit' (it isn't restructuring
-- anything).
module Storyteller.Core.Annotation
  ( addNote
  ) where

import qualified Data.Text as T
import Polysemy
import Polysemy.Fail

import Storyteller.Core.Storage (StoryBranch, StoryStorage, follow, storeAs)
import Storyteller.Core.Types (TickId(..), Note(..), tickId, tickParent)

-- | Add an annotation note referencing zero or more existing ticks — zero
--   is valid, a free-floating remark rather than a comment on any specific
--   atom.
addNote
  :: forall branch r
  .  Members '[StoryBranch branch, StoryStorage, Fail] r
  => [TickId] -> T.Text -> Sem r ()
addNote refs text = do
  ticks <- follow @branch [] $ \acc t -> (t : acc, tickParent t)
  let known   = map tickId ticks
      missing = filter (`notElem` known) refs
  case missing of
    (bad : _) -> fail $ "ref tick not found: " <> T.unpack (unTickId bad)
    []        -> () <$ storeAs @branch (Note refs text)
