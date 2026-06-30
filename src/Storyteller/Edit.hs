{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
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

    -- * Working-tree commit
  , commitWorkingTree
  , AppendBlock(..)
  , DiffError(..)

    -- * Ordering invariant check (exported for use in Dispatch)
  , checkMoveOrder

    -- * Exported for tests
  , computeBlocks
  , deriveHistory
  , blocksFromTimeline
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

import qualified Data.List as List
import Runix.FileSystem
  ( FileSystem, FileSystemRead, FileSystemWrite
  , appendFile, fileExists, readFile, writeFile, listFiles, isDirectory
  )
import Storyteller.Git (BranchTag)
import Storyteller.Storage
  ( StoryBranch, StoryStorage
  , at, atWithFS, withFS, drop, reset, store, storeData, follow, get, updateReferences
  )
import Storyteller.Types (TickId(..), Tick(..), TickData(..), TickType(..), tickId, tickParent)

import Prelude hiding (appendFile, drop, get, readFile, writeFile)

-- ---------------------------------------------------------------------------
-- Position-free tick representation
-- ---------------------------------------------------------------------------

-- | A tick extracted from the chain, ready to be re-inserted elsewhere.
--   Carries the metadata (message, refs) and the concrete file diffs the
--   tick introduced, so it can be faithfully replayed at a new position
--   without depending on the current WorkingTree state.
data TDraft = TDraft
  { tdRefs      :: [TickId]
  , tdFields    :: [(T.Text, T.Text)]
  , tdMessage   :: T.Text
  , tdFileDiffs :: Map FilePath BS.ByteString  -- ^ per-file suffix this tick added
  } deriving (Show, Eq)

toTickData :: TDraft -> TickData
toTickData d = TickData { tickRefs = tdRefs d, tickFields = tdFields d, tickMessage = tdMessage d }

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
    -- Strip atom-specific storage fields: "tree" (pre-built git tree hash) and
    -- "file" (path hint). Both are position-dependent and must not carry over
    -- when the tick is re-inserted at a new chain position by pushTick.
    -- Keeping "tree" would cause Store to reuse the old hash, writing the wrong
    -- content to the rebased tick.
    , tdFields    = filter ((`notElem` ["tree", "file"]) . fst) (tickFields (tickData tick))
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
                , FileSystem      (BranchTag branch)
                , FileSystemRead  (BranchTag branch)
                , FileSystemWrite (BranchTag branch)
                , StoryStorage
                , Fail ] r )
  => TickId
  -> FilePath
  -> BS.ByteString
  -> Sem r (TickId, [(TickId, TickId)])
editAtom tid path newBytes = do
  (newTid, mapping) <- at @branch tid $ do
    drop @branch
    withFS @branch $ do
      writeFile @(BranchTag branch) path newBytes
      store @branch "edit"
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

-- ---------------------------------------------------------------------------
-- Working-tree commit (formerly SplitDiffMerge)
-- ---------------------------------------------------------------------------

-- | A block of new bytes to be inserted after a specific tick.
data AppendBlock = AppendBlock
  { blockFile      :: FilePath
  , blockAfterTick :: TickId
  , blockContent   :: BS.ByteString
  } deriving (Show, Eq)

data DiffError
  = ModificationDetected FilePath Int Int
  | GapDetected FilePath Int
  deriving (Show, Eq)

-- | Inspect the current working tree, diff it against the committed history,
--   and insert each block of new content at the correct position in the chain
--   using 'at'. Handles arbitrary mid-chain insertions in a single pass.
--   Returns the complete old→new tick id mapping.
commitWorkingTree
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[ FileSystem      project
                , FileSystemRead  project
                , FileSystemWrite project
                , StoryBranch branch
                , StoryStorage
                , Fail ] r )
  => Sem r [(TickId, TickId)]
commitWorkingTree = do
  files    <- listFiles @project "/"
  history  <- buildFileHistory @project @branch
  working  <- Map.fromList <$> mapM (\f -> (f,) <$> readWorking @project f) files
  blocks   <- either (fail . renderDiffError) return (computeBlocks history working)
  if null blocks then return [] else applyBlocks @project @branch blocks

type FileHistory = Map FilePath [(TickId, Int)]

buildFileHistory
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[StoryBranch branch, FileSystemRead project, FileSystem project, Fail] r )
  => Sem r FileHistory
buildFileHistory = do
  ticks     <- follow @branch [] $ \acc tick -> (tick : acc, tickParent tick)
  snapshots <- mapM (readSnapshotAt @project @branch) (reverse ticks)
  return $ deriveHistory snapshots

readSnapshotAt
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[StoryBranch branch, FileSystemRead project, FileSystem project, Fail] r )
  => Tick
  -> Sem r (TickId, Map FilePath BS.ByteString)
readSnapshotAt tick = do
  (fileMap, _) <- atWithFS @branch (tickId tick) $ do
    files <- listFiles @project "/"
    Map.fromList <$> mapM (\f -> (f,) <$> readFile @project f) files
  return (tickId tick, fileMap)

deriveHistory :: [(TickId, Map FilePath BS.ByteString)] -> FileHistory
deriveHistory snapshots =
  let allFiles = List.nub [ f | (_, m) <- snapshots, f <- Map.keys m ]
  in Map.fromList [ (f, fileTimeline f snapshots) | f <- allFiles ]
  where
    fileTimeline file snaps =
      [ (tid, BS.length content)
      | (tid, fileMap) <- snaps
      , Just content <- [Map.lookup file fileMap]
      ]

computeBlocks
  :: FileHistory
  -> Map FilePath BS.ByteString
  -> Either DiffError [AppendBlock]
computeBlocks history working =
  fmap concat $ mapM (fileBlocks history) (Map.toList working)

fileBlocks
  :: FileHistory
  -> (FilePath, BS.ByteString)
  -> Either DiffError [AppendBlock]
fileBlocks history (file, workingContent) =
  case Map.lookup file history of
    Nothing -> Right []
    Just timeline ->
      let committedLength = case timeline of { [] -> 0; xs -> snd (last xs) }
          workingLength   = BS.length workingContent
      in if workingLength < committedLength
           then Left (ModificationDetected file committedLength workingLength)
           else blocksFromTimeline file timeline workingContent

blocksFromTimeline
  :: FilePath
  -> [(TickId, Int)]
  -> BS.ByteString
  -> Either DiffError [AppendBlock]
blocksFromTimeline file timeline workingContent = go 0 timeline []
  where
    go _prevLen [] acc =
      let lastLen = case timeline of { [] -> 0; xs -> snd (last xs) }
          suffix  = BS.drop lastLen workingContent
      in if BS.null suffix then Right (reverse acc)
         else case timeline of
           [] -> Right (reverse acc)
           xs -> Right (reverse (AppendBlock file (fst (last xs)) suffix : acc))
    go prevLen ((tid, cumulativeLen) : rest) acc =
      let available = BS.length workingContent - prevLen
      in if available <= 0 then Right (reverse acc)
         else go cumulativeLen rest acc

applyBlocks
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[StoryBranch branch, FileSystemRead project, FileSystemWrite project,
                 FileSystem project, StoryStorage, Fail] r )
  => [AppendBlock]
  -> Sem r [(TickId, TickId)]
applyBlocks = fmap concat . mapM (applyBlock @project @branch)

applyBlock
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[StoryBranch branch, FileSystemRead project, FileSystemWrite project,
                 FileSystem project, StoryStorage, Fail] r )
  => AppendBlock
  -> Sem r [(TickId, TickId)]
applyBlock (AppendBlock file afterTick content) = do
  (_tid, mapping) <- at @branch afterTick $ do
    appendFile @project file content
    store @branch ("atom: " <> T.pack file)
  return mapping

readWorking
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => FilePath -> Sem r BS.ByteString
readWorking path = do
  exists <- fileExists @project path
  if exists then readFile @project path else return BS.empty

renderDiffError :: DiffError -> String
renderDiffError (ModificationDetected f committed working) =
  "commitWorkingTree: " <> f <> " shrinks relative to committed content"
  <> " (committed=" <> show committed <> " working=" <> show working <> ")"
renderDiffError (GapDetected f offset) =
  "commitWorkingTree: " <> f <> " has a content gap at byte offset " <> show offset
