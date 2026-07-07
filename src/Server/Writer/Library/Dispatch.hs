{-# LANGUAGE FlexibleContexts #-}

-- | Dispatch for @\/library\/{name}@ connections.
--
-- Routing only: decode 'LibraryCommand' → call 'Server.Writer.Library'. No
-- business logic here. Runs against the ambient, already-open branch scope
-- ('BranchOpen') — see 'Server.Writer.Library.Connection' for where that
-- scope is entered.
--
-- Returns no events of its own: a successful 'ChapterCreate' reaches every
-- open library connection (this one included) via the same ref-move
-- notification any other write does — see
-- 'Server.Writer.Library.Connection.onNotify'. There's nothing this
-- dispatch can convey that the next full-tree push wouldn't already carry.
module Server.Writer.Library.Dispatch
  ( runCommand
  ) where

import Polysemy (Sem)

import Server.Core.Branch (BranchOpen)
import Server.Writer.Library (chapterCreate)
import Server.Writer.Library.Protocol (LibraryCommand(..), LibraryEvent)

runCommand :: BranchOpen r => LibraryCommand -> Sem r [LibraryEvent]
runCommand (ChapterCreate path name) = do
  chapterCreate path name
  return []
