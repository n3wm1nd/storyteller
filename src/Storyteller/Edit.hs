{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Chain editing operations: delete, edit, and move ticks in a branch.
--
-- Composed from storage primitives (At, Drop, WithFS, Reset). No LLM or
-- splitter involvement.
--
-- == Move semantics
--
-- Moving tick A to after tick B is always a single nested At.  Two cases:
--
--   Backward (A currently after B):
--     at A $ do d <- popTick A; at B $ pushTick d
--
--   Forward (A currently before B):
--     at B $ do (d, _) <- at A $ popTick A; pushTick d
--
-- popTick reads the file diff the tick introduced (via two atWithFS reads:
-- the tick's snapshot minus its parent's snapshot) so that pushTick can
-- re-apply exactly those bytes at the new position, regardless of what the
-- outer WorkingTree state is.
module Storyteller.Edit
  ( -- * Position-free tick representation
    TDraft(..)
  , popTick
  , pushTick

    -- * Chain editing operations
  , deleteTick
  , editAtom
  , moveTick

    -- * Ordering invariant check (exported for use in Dispatch)
  , checkMoveOrder
  ) where

import qualified Data.ByteString as BS
import Control.Monad (foldM)
import Data.List (findIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Polysemy
import Polysemy.Fail

import Runix.FileSystem
  ( FileSystem, FileSystemRead, FileSystemWrite
  , appendFile, readFile, listFiles, isDirectory
  )
import Storyteller.Atom (AtomDiff(..), storeAtomDiff, treeRef)
import Storyteller.Git (BranchTag)
import Storyteller.Storage
  ( StoryBranch, StoryStorage
  , at, withFS, drop, reset, store, storeData, storeAs, follow, get, updateReferences
  )
import Storyteller.Types (TickId(..), Tick(..), TickData(..), TickType(..), tickId, tickParent)
import Runix.Git (Git)

import Prelude hiding (appendFile, drop, get, readFile)

-- ---------------------------------------------------------------------------
-- Position-free tick representation
-- ---------------------------------------------------------------------------

-- | A tick extracted from the chain, ready to be re-inserted elsewhere.
--   Carries the metadata (message, refs) and the concrete file diffs the
--   tick introduced, so it can be faithfully replayed at a new position
--   without depending on the current WorkingTree state.
data TDraft = TDraft
  { tdRefs      :: [TickId]
  , tdMessage   :: T.Text
  , tdFileDiffs :: Map FilePath BS.ByteString  -- ^ per-file suffix this tick added
  } deriving (Show, Eq)

toTickData :: TDraft -> TickData
toTickData d = TickData { tickRefs = tdRefs d, tickFields = [], tickMessage = tdMessage d }

remapDraftRefs :: [(TickId, TickId)] -> TDraft -> TDraft
remapDraftRefs mapping d = d { tdRefs = map remap (tdRefs d) }
  where remap tid = maybe tid id (lookup tid mapping)

-- | Pop the tick currently at HEAD, returning its draft with file diffs.
--   Must be called as the inner action of @at tid@ — that puts @tid@ at HEAD.
--
--   Uses withFS to snapshot HEAD's files, drops to the parent, then snapshots
--   again. The diff is the suffix each file gained in this tick.
popTick
  :: forall branch r
  .  ( Members '[ StoryBranch branch
                , FileSystem      (BranchTag branch)
                , FileSystemRead  (BranchTag branch)
                , StoryStorage
                , Fail ] r )
  => Sem r TDraft
popTick = do
  tick <- get @branch

  -- Snapshot at HEAD (this tick's tree).
  tickFiles <- withFS @branch $ readAllFiles @branch

  -- Rewind to parent.
  drop @branch

  -- Snapshot at the new HEAD (this tick's parent's tree).
  parentFiles <- withFS @branch $ readAllFiles @branch

  -- Diff: bytes this tick added beyond its parent, per file.
  let diffs = Map.mapWithKey
        (\path tickContent ->
          let parentContent = Map.findWithDefault BS.empty path parentFiles
          in  BS.drop (BS.length parentContent) tickContent)
        tickFiles

  return TDraft
    { tdRefs      = tickRefs (tickData tick)
    , tdMessage   = tickMessage (tickData tick)
    , tdFileDiffs = diffs
    }

-- | Re-insert a popped tick at the current head by appending its file diffs
--   and committing with the original message and refs.
--   The current WorkingTree is irrelevant — we apply the diffs on top of
--   whatever the head commit's snapshot is (via withFS), then store.
pushTick
  :: forall branch r
  .  ( Members '[ StoryBranch branch
                , FileSystem      (BranchTag branch)
                , FileSystemRead  (BranchTag branch)
                , FileSystemWrite (BranchTag branch)
                , StoryStorage
                , Fail ] r )
  => TDraft -> Sem r TickId
pushTick d = withFS @branch $ do
  mapM_ (\(path, suffix) -> appendFile @(BranchTag branch) path suffix)
        (Map.toList (tdFileDiffs d))
  storeData @branch (toTickData d)

-- | Remove a tick from the chain entirely.
--   Returns the old→new id mapping for all replayed ticks.
deleteTick
  :: forall branch r
  .  Members '[StoryBranch branch, StoryStorage, Fail] r
  => TickId
  -> Sem r [(TickId, TickId)]
deleteTick tid = do
  (_unit, mapping) <- at @branch tid $ drop @branch
  reset @branch
  return mapping

-- | Replace an atom's content in-place, preserving its chain position.
--   Returns (newTickId, tail-mapping).
editAtom
  :: forall branch r
  .  ( Members '[ StoryBranch branch
                , StoryStorage
                , Git
                , Fail ] r )
  => TickId
  -> FilePath
  -> BS.ByteString
  -> Sem r (TickId, [(TickId, TickId)])
editAtom tid path newBytes = do
  (newTid, mapping) <- at @branch tid $ do
    drop @branch
    parentTick <- get @branch
    parentTree <- treeRef (tickId parentTick)
    atom       <- storeAtomDiff parentTree (AtomDiff path newBytes)
    storeAs @branch atom
  reset @branch
  return (newTid, mapping)

-- ---------------------------------------------------------------------------
-- Chain-level move
-- ---------------------------------------------------------------------------

-- | Move @tid@ to immediately after @mAfter@ (@Nothing@ = move to front).
--
--   Backward (tid currently after target):
--     @at tid $ do d <- popTick tid; at after $ pushTick d@
--
--   Forward (tid currently before target):
--     @at after $ do (d,_) <- at tid $ popTick tid; pushTick d@
--
--   Both are a single nested At — one coherent rebase pass.
--   Returns the complete old→new id mapping for every tick that changed.
moveTick
  :: forall branch r
  .  ( Members '[ StoryBranch branch
                , FileSystem      (BranchTag branch)
                , FileSystemRead  (BranchTag branch)
                , FileSystemWrite (BranchTag branch)
                , StoryStorage
                , Fail ] r )
  => TickId
  -> Maybe TickId
  -> Sem r [(TickId, TickId)]
moveTick tid mAfter = do
  chain <- follow @branch [] (\acc t -> (t : acc, tickParent t))
  -- chain is oldest-first: [root, t1, t2, t3]
  let root           = head chain
      contentOrdered = tail chain

  tidPos   <- findPos "tick to move" tid contentOrdered
  afterPos <- case mAfter of
    Nothing  -> return (-1)
    Just aid -> findPos "afterTickId" aid contentOrdered

  checkMoveOrder tid mAfter tidPos afterPos contentOrdered

  let resolvedAfter = maybe (tickId root) id mAfter

  ((newTid, innerMapping), outerMapping) <-
    if tidPos > afterPos
      then -- Backward: at tid (pop >>= \d -> at after (push d))
        at @branch tid $ do
          d                  <- popTick @branch
          (newTid, innerMap) <- at @branch resolvedAfter $ pushTick @branch d
          return (newTid, innerMap)
      else -- Forward: at after (at tid pop >>= push)
        at @branch resolvedAfter $ do
          (d, innerMap) <- at @branch tid $ popTick @branch
          newTid        <- pushTick @branch d
          return (newTid, innerMap)

  reset @branch
  let fullMapping = outerMapping <> innerMapping <> [(tid, newTid)]
  updateReferences fullMapping
  return fullMapping

-- ---------------------------------------------------------------------------
-- Ordering invariant
-- ---------------------------------------------------------------------------

checkMoveOrder
  :: Members '[Fail] r
  => TickId -> Maybe TickId -> Int -> Int -> [Tick] -> Sem r ()
checkMoveOrder tid _mAfter tidPos afterPos ordered = do
  let movingTick = ordered !! tidPos
      newPos     = afterPos + 1

  mapM_ (checkRefBefore newPos) (tickRefs (tickData movingTick))

  let precedingSlice = filter (\t -> tickId t /= tid) (take (afterPos + 1) ordered)
  mapM_ (checkNotRefTo tid) precedingSlice
  where
    checkRefBefore newPos refId =
      case findIndex (\t -> tickId t == refId) ordered of
        Nothing -> return ()
        Just rp ->
          if rp < newPos then return ()
          else fail $ "cannot move tick before its own reference "
                    <> T.unpack (unTickId refId)

    checkNotRefTo movingId t =
      if movingId `elem` tickRefs (tickData t)
        then fail $ "cannot move tick after tick that references it: "
                  <> T.unpack (unTickId (tickId t))
        else return ()

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

findPos :: Members '[Fail] r => String -> TickId -> [Tick] -> Sem r Int
findPos label tid ordered =
  maybe (fail $ label <> " not found: " <> T.unpack (unTickId tid)) return
    (findIndex (\t -> tickId t == tid) ordered)

-- | Read all file paths and their contents from the current WorkingTree snapshot.
--   Recursively lists directories. Only returns files (not directory entries).
readAllFiles
  :: forall branch r
  .  Members '[FileSystem (BranchTag branch), FileSystemRead (BranchTag branch), Fail] r
  => Sem r (Map FilePath BS.ByteString)
readAllFiles = go "/" Map.empty
  where
    go dir acc = do
      entries <- listFiles @(BranchTag branch) dir
      foldM (visit dir) acc entries

    visit dir acc name = do
      let path = if dir == "/" then name else dir <> "/" <> name
      isDir <- isDirectory @(BranchTag branch) path
      if isDir
        then go path acc
        else do
          content <- readFile @(BranchTag branch) path
          return (Map.insert path content acc)
