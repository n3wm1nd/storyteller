{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Reading a curated recent slice of a character's journal.
--
-- The curation itself is 'Storage.Tick.recentAtomsOf' (entries
-- byte-identical to what they reference are dropped, kept ones bring
-- neighbours along); what this module adds is the vocabulary around it —
-- a named 'JournalCuration' instead of three transposable 'Int's, and a
-- result that is the entries' own text rather than raw
-- 'Storage.Tick.FileTick's, so a caller asking \"what's new in the
-- journal\" never has to know about @ftKind@ or the hide flag.
--
-- No position parameter, and there will not be one: reading /another/
-- character's journal means entering their branch
-- ('Storyteller.Core.Branch.withBranch') and calling this inside, the same
-- as any other cross-branch read in this codebase.
module Storyteller.Writer.Journal
  ( JournalCuration(..)
  , journalWindow
  ) where

import Data.Text (Text)
import Polysemy

import qualified Storage.Tick as Tick
import Storyteller.Core.Branch (BranchOp, runStorage)

-- | See 'Storage.Tick.recentAtomsOf' for what each field actually curates.
data JournalCuration = JournalCuration
  { lookback :: Int
  , maxOut   :: Int
  , padding  :: Int
  } deriving (Eq, Show)

-- | @path@'s curated recent entries, oldest first, as their own message
--   text. Callers wrap this into whatever presentation shape they need (a
--   DSL 'Storyteller.Context.DSL.Value.Message', a
--   'Storyteller.Writer.Agent.CharContext.CharContextBlock', ...) with
--   their own framing header, rather than this committing to one.
journalWindow
  :: forall branch r
  .  Member (BranchOp branch) r
  => FilePath -> JournalCuration -> Sem r [Text]
journalWindow path curation =
  map Tick.ftMessage <$> runStorage @branch
    (Tick.recentAtomsOf path (lookback curation) (maxOut curation) (padding curation))
