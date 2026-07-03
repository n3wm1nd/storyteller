{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | File-level business logic.
--
-- These functions assume the branch's storage/filesystem scope ('FileOpen')
-- is already live in the ambient stack. The connection (see
-- 'Server.File.Connection') reopens that scope fresh around each command,
-- nested inside a 'Storyteller.Git.withStorage' transaction, so a command's
-- writes are all-or-nothing and visible immediately, not just at
-- disconnect — these functions don't need to know that; they just see
-- 'FileOpen' as already open.
--
-- No JSON, no WebSocket, no T.Text ids — callers handle the boundary.
-- These functions are the unit under test.
module Server.File
  ( FileOpen
  , fileState
  , fileStateSince
  , appendToFile
  , editFileAtom
  , deleteFileAtom
  , moveFileAtom
  , chatWriter
  , chatFixer
  ) where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy (Members, Member, Sem)
import Polysemy.Fail (Fail)
import Runix.Logging (info)

import Server.Protocol (Update(..), toWireTick)
import Server.File.Protocol (ContextItem(..))
import Server.Run (SessionEffects)

import Storyteller.Agent (Prompt(..), Instruction(..), ContextBlock(..))
import Storyteller.Agent.Append (appendUnsplit)
import Storyteller.Agent.Splitter (Splitter)
import Storyteller.Agent.Write (writeAgent)
import Storyteller.Agent.FlowWrite (flowWriteAgent)
import Storyteller.Agent.Fix (fixAgent)
import Storyteller.Runtime (Main)
import qualified Storyteller.Storage as Storage
import Storyteller.Storage (FileTick, StoryBranch, StoryStorage, fileTicks, storeAs)
import Storyteller.Edit (deleteTick, editAtom, moveTick)
import Storyteller.Types (TickId(..))
import Storyteller.Git (BranchTag)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)

-- | The effects live once a file connection has entered its branch's scope —
--   one 'StoryBranch'/filesystem instance for the connection's whole
--   lifetime, not reopened per command.
type FileOpen r =
  Members '[ StoryBranch Main
           , StoryStorage
           , FileSystemWrite (BranchTag Main)
           , FileSystemRead  (BranchTag Main)
           , FileSystem      (BranchTag Main)
           , Fail
           ] r

-- ---------------------------------------------------------------------------
-- State query
-- ---------------------------------------------------------------------------

-- | Full file state: all ticks for this path and current HEAD id.
--   An absent file (no ticks yet) is an empty 'Update' (head = "").
fileState :: FileOpen r => FilePath -> Sem r Update
fileState path = fileStateSince path Nothing

-- | File state, optionally incremental. When 'since' names a tick still
--   present in this file's chain, only ticks after it are included. When
--   'since' is 'Nothing' or no longer present (rewritten out from under it),
--   the full chain is returned.
fileStateSince :: FileOpen r => FilePath -> Maybe T.Text -> Sem r Update
fileStateSince path since = fileUpdateSince since <$> fileTicks @Main path

-- ---------------------------------------------------------------------------
-- Mutations on the already-open branch
-- ---------------------------------------------------------------------------

-- | Append content to a file as a single, unsplit atom — the caller
--   (someone typing and appending their own text) already chose exactly
--   what they wanted stored; paragraph-splitting is for generated prose
--   (see 'chatPrompt'), not for this.
appendToFile :: (FileOpen r, SessionEffects r) => FilePath -> T.Text -> Sem r ()
appendToFile path content = do
  info $ "appending to: " <> T.pack path
  void $ appendUnsplit @Main path content
  info $ "append done: " <> T.pack path

-- | Replace an atom's content in-place. 'editAtom' broadcasts its own
--   old->new mapping (including the edited tick's own pivot pair) via
--   'Storyteller.Storage.at', so there's nothing left to do here.
editFileAtom :: FileOpen r => FilePath -> TickId -> T.Text -> Sem r ()
editFileAtom path tid content = void $ editAtom @Main tid path (TE.encodeUtf8 content)

-- | Delete an atom from the file's chain. 'deleteTick' broadcasts its own
--   mapping via 'Storyteller.Storage.at', so there's nothing left to do here.
deleteFileAtom :: FileOpen r => TickId -> Sem r ()
deleteFileAtom tid = void $ deleteTick @Main tid

-- | Move an atom to a new position in the file's chain.
moveFileAtom :: FileOpen r => TickId -> Maybe TickId -> Sem r ()
moveFileAtom tid mAfter = void $ moveTick @Main tid mAfter

-- | Store a prompt tick then run Writer, or FlowWriter when 'mFlowTid' is
--   set (the tick that was HEAD when the user started typing — see
--   'Storyteller.Agent.FlowWrite').
chatWriter :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> T.Text -> [ContextItem] -> Maybe TickId -> Sem r ()
chatWriter path prompt context mFlowTid = do
  _ <- storeAs @Main (Prompt path prompt)
  let ctxBlocks = toContextBlocks context
  case mFlowTid of
    Just flowTid -> do
      info $ "flow writer agent starting: " <> T.pack path
      _ <- flowWriteAgent @(BranchTag Main) @Main path flowTid (Instruction prompt) ctxBlocks []
      info $ "flow writer agent done: " <> T.pack path
    Nothing -> do
      info $ "writer agent starting: " <> T.pack path
      _ <- writeAgent @(BranchTag Main) @Main path (Instruction prompt) ctxBlocks []
      info $ "writer agent done: " <> T.pack path

-- | Store a prompt tick then run the Fixer agent against the given targets.
chatFixer :: (FileOpen r, Member Splitter r, SessionEffects r) => FilePath -> T.Text -> [ContextItem] -> [TickId] -> Sem r ()
chatFixer path prompt context targets = do
  _ <- storeAs @Main (Prompt path prompt)
  info $ "fixer agent starting: " <> T.pack path
  _ <- fixAgent @(BranchTag Main) @Main path targets (Instruction prompt) (toContextBlocks context)
  info $ "fixer agent done: " <> T.pack path

toContextBlocks :: [ContextItem] -> [ContextBlock]
toContextBlocks = map (ContextBlock . ciContent)

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

fileUpdateSince :: Maybe T.Text -> [FileTick] -> Update
fileUpdateSince since ticks = Update
  { updateTicks = map toWireTick (Storage.ticksSince since ticks)
  , updateHead  = case reverse ticks of
                    []    -> ""
                    (t:_) -> Storage.ftTickId t
  }
