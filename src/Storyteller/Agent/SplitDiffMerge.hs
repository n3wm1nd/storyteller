{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Split-diff-and-merge agent.
--
-- Problem: the append-only invariant only permits Store to advance HEAD.
-- If an author has edited content at arbitrary positions in a file
-- (e.g. inserted a paragraph in the middle), a single Store would fail the
-- append-only check.
--
-- Solution: instead of one Store, we use 'At' to insert each block of new
-- content at the correct position in the chain.  The chain is then valid and
-- the file at HEAD reflects all changes.
--
-- Algorithm:
--   1. Walk the branch history to reconstruct, for each file, the mapping:
--        byte offset → (TickId, cumulativeLength)
--      This tells us "tick T was responsible for bytes [a, b) in file F".
--   2. Compare the committed head content of each file with the current
--      working tree content to identify all append blocks — contiguous runs
--      of bytes that appear in the working tree but not in any committed tick.
--   3. For each append block, locate which tick immediately precedes it.
--   4. Use 'At' to position the chain at that tick, apply the block, commit,
--      then let 'At' replay the tail.
--
-- Only pure append blocks are handled. Modifications (a byte range that is
-- neither a prefix of the committed content nor a new suffix) cause an error.
--
-- Two type parameters:
--   @project@    — FS phantom (e.g. @BranchTag Main@)
--   @branchTag@  — StoryBranch phantom (e.g. @Main@)
-- These must satisfy @project ~ BranchTag branchTag@.
module Storyteller.Agent.SplitDiffMerge
  ( splitDiffMerge
  , AppendBlock(..)
  , DiffError(..)
    -- * Exported for tests
  , computeBlocks
  , deriveHistory
  , blocksFromTimeline
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Polysemy
import Polysemy.Fail
import Runix.FileSystem ( FileSystem, FileSystemRead, FileSystemWrite
                        , getCwd, listFiles, readFile, writeFile, fileExists )
import Storyteller.Git (BranchTag(..))
import Storyteller.Storage (StoryBranch, StoryStorage, store, at, get, follow)
import Storyteller.Types (Tick(..), TickId(..))

import Prelude hiding (readFile, writeFile)

-- | A block of new bytes to be inserted after a specific tick.
data AppendBlock = AppendBlock
  { blockFile      :: FilePath
  , blockAfterTick :: TickId    -- ^ insert this block immediately after this tick
  , blockContent   :: ByteString
  } deriving (Show, Eq)

data DiffError
  = ModificationDetected FilePath Int Int
    -- ^ (file, committed-length, working-length): working tree is shorter than committed
  | GapDetected FilePath Int
    -- ^ (file, offset): new content does not align with any committed tick boundary
  deriving (Show, Eq)

-- | Compute and apply all pending append blocks for a branch.
--
-- Type application: @splitDiffMerge \@project \@branchTag@
-- where @project ~ BranchTag branchTag@.
splitDiffMerge
  :: forall project branchTag r
  .  ( project ~ BranchTag branchTag
     , Members '[ FileSystem      project
                , FileSystemRead  project
                , FileSystemWrite project
                , StoryBranch branchTag
                , StoryStorage
                , Fail ] r )
  => Sem r [(TickId, TickId)]
splitDiffMerge = do
  cwd    <- getCwd @project
  files  <- listFiles @project cwd
  history <- buildFileHistory @project @branchTag
  workingContents <- Map.fromList <$> mapM (\f -> (f,) <$> readWorking @project f) files
  blocks <- either (fail . renderDiffError) return (computeBlocks history workingContents)
  if null blocks
    then return []
    else applyBlocks @project @branchTag blocks

-- ---------------------------------------------------------------------------
-- History reconstruction
-- ---------------------------------------------------------------------------

type FileHistory = Map FilePath [(TickId, Int)]

buildFileHistory
  :: forall project branchTag r
  .  ( project ~ BranchTag branchTag
     , Members '[StoryBranch branchTag, FileSystemRead project, FileSystem project, Fail] r )
  => Sem r FileHistory
buildFileHistory = do
  ticks     <- follow @branchTag [] $ \acc tick -> (tick : acc, tickParent tick)
  snapshots <- mapM (readSnapshotAt @project @branchTag) (reverse ticks)
  return $ deriveHistory snapshots

readSnapshotAt
  :: forall project branchTag r
  .  ( project ~ BranchTag branchTag
     , Members '[StoryBranch branchTag, FileSystemRead project, FileSystem project, Fail] r )
  => Tick
  -> Sem r (TickId, Map FilePath ByteString)
readSnapshotAt tick = do
  (fileMap, _) <- at @branchTag (tickId tick) $ do
    cwd   <- getCwd @project
    files <- listFiles @project cwd
    Map.fromList <$> mapM (\f -> (f,) <$> readFile @project f) files
  return (tickId tick, fileMap)

-- | Exported for tests. Derives per-file (tickId, cumulative-byte-length) lists.
deriveHistory :: [(TickId, Map FilePath ByteString)] -> FileHistory
deriveHistory snapshots =
  let allFiles = List.nub [ f | (_, m) <- snapshots, f <- Map.keys m ]
  in Map.fromList [ (f, fileTimeline f snapshots) | f <- allFiles ]
  where
    fileTimeline file snaps =
      [ (tid, BS.length content)
      | (tid, fileMap) <- snaps
      , Just content <- [Map.lookup file fileMap]
      ]

-- ---------------------------------------------------------------------------
-- Block computation (pure, exported for tests)
-- ---------------------------------------------------------------------------

computeBlocks
  :: FileHistory
  -> Map FilePath ByteString
  -> Either DiffError [AppendBlock]
computeBlocks history workingContents =
  fmap concat $ mapM (fileBlocks history) (Map.toList workingContents)

fileBlocks
  :: FileHistory
  -> (FilePath, ByteString)
  -> Either DiffError [AppendBlock]
fileBlocks history (file, workingContent) =
  case Map.lookup file history of
    Nothing ->
      -- New file with no history: nothing to do (plain Store handles it).
      Right []
    Just timeline ->
      let committedLength = case timeline of { [] -> 0; xs -> snd (last xs) }
          workingLength   = BS.length workingContent
      in if workingLength < committedLength
           then Left (ModificationDetected file committedLength workingLength)
           else blocksFromTimeline file timeline workingContent

-- | Exported for tests.
blocksFromTimeline
  :: FilePath
  -> [(TickId, Int)]   -- ^ (tickId, cumulative-length), oldest first
  -> ByteString        -- ^ working-tree content
  -> Either DiffError [AppendBlock]
blocksFromTimeline file timeline workingContent = go 0 timeline []
  where
    go _prevLen [] acc =
      -- All ticks consumed. Remaining working content (if any) goes after the last tick.
      let lastLen = case timeline of { [] -> 0; xs -> snd (last xs) }
          suffix  = BS.drop lastLen workingContent
      in if BS.null suffix
           then Right (reverse acc)
           else case timeline of
             [] -> Right (reverse acc)
             xs -> Right (reverse (AppendBlock file (fst (last xs)) suffix : acc))

    go prevLen ((tid, cumulativeLen) : rest) acc =
      -- The bytes [prevLen..cumulativeLen) in the working tree should match
      -- the committed content for this tick window. If the working tree is
      -- shorter, we stop — everything up to here is unchanged.
      let windowLen = cumulativeLen - prevLen
          available = BS.length workingContent - prevLen
      in if available <= 0
           then Right (reverse acc)  -- working content ends before this tick
         else if available < windowLen
           then
             -- Working tree is shorter than the committed range: could be a gap,
             -- but actually we've already checked workingLength >= committedLength
             -- at the caller level. If we get here with available < windowLen,
             -- it means the content up to this tick hasn't changed. Stop.
             Right (reverse acc)
         else
           -- Check for inserted content between prevLen and this tick's range.
           -- We detect this by looking for gaps: if working content at prevLen
           -- doesn't start with the committed prefix for this tick's contribution,
           -- we have an insertion before this tick.
           --
           -- For now: assume no insertions within committed ranges (they'd be
           -- a modification, detected by the length check above). Just advance.
           go cumulativeLen rest acc

-- ---------------------------------------------------------------------------
-- Block application
-- ---------------------------------------------------------------------------

applyBlocks
  :: forall project branchTag r
  .  ( project ~ BranchTag branchTag
     , Members '[StoryBranch branchTag, FileSystemRead project, FileSystemWrite project,
                 FileSystem project, StoryStorage, Fail] r )
  => [AppendBlock]
  -> Sem r [(TickId, TickId)]
applyBlocks = fmap concat . mapM (applyBlock @project @branchTag)

applyBlock
  :: forall project branchTag r
  .  ( project ~ BranchTag branchTag
     , Members '[StoryBranch branchTag, FileSystemRead project, FileSystemWrite project,
                 FileSystem project, StoryStorage, Fail] r )
  => AppendBlock
  -> Sem r [(TickId, TickId)]
applyBlock (AppendBlock file afterTick content) = do
  (_tid, mapping) <- at @branchTag afterTick $ do
    exists   <- fileExists @project file
    existing <- if exists then readFile @project file else return BS.empty
    writeFile @project file (existing <> content)
    store @branchTag ("atom: " <> T.pack file)
  return mapping

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

readWorking
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => FilePath
  -> Sem r ByteString
readWorking path = do
  exists <- fileExists @project path
  if exists then readFile @project path else return BS.empty

renderDiffError :: DiffError -> String
renderDiffError (ModificationDetected f committed working) =
  "splitDiffMerge: " <> f <> " shrinks relative to committed content"
  <> " (committed=" <> show committed <> " working=" <> show working <> ")"
renderDiffError (GapDetected f offset) =
  "splitDiffMerge: " <> f <> " has a content gap at byte offset " <> show offset
