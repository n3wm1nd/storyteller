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
module Storyteller.Core.Edit
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
  , commitFiles

    -- * Ordering invariant check (exported for use in Dispatch)
  , checkMoveOrder
  ) where

import qualified Data.ByteString as BS
import Control.Monad (foldM, filterM)
import Data.Array (Array, listArray, (!))
import Data.List (findIndex, zip5)
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
  , appendFile, writeFile, fileExists, readFile, listFiles, isDirectory
  )
import Storyteller.Core.Atom (Atom(..))
import Storyteller.Core.Git (BranchTag)
import Storyteller.Core.Storage
  ( StoryBranch, StoryStorage
  , at, sneakyAt, sneakyAtWithFS, atWithFS, readAtWithFS, withFS, drop, reset, store, storeAs, storeData, follow, get, updateReferences
  )
import Storyteller.Core.Types (TickId(..), Tick(..), TickData(..), TickType(..), tickId, tickParent)

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
    , tdFields    = tickFields (tickData tick)
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
  (newTid, mapping) <- sneakyAt @branch tid $ do
    drop @branch
    withFS @branch $ do
      -- 'drop' rewinds the branch's (temporary) head to tid's parent; withFS
      -- loads that head's tree as the working tree, so the file here holds
      -- every preceding atom's content already. Append the new atom content
      -- rather than overwrite, so this tick's diff is just the one atom
      -- being edited — matching the append-only invariant Store checks.
      appendFile @(BranchTag branch) path newBytes
      storeAs @branch (Atom path (TE.decodeUtf8With TE.lenientDecode newBytes))
  reset @branch
  let fullMapping = (tid, newTid) : mapping
  updateReferences fullMapping
  return (newTid, fullMapping)

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
  (root, contentOrdered) <- case chain of
    (r : rest) -> return (r, rest)
    []         -> fail "moveTick: branch has no root tick"

  tidPos   <- findPos "tick to move" tid contentOrdered
  afterPos <- case mAfter of
    Nothing               -> return (-1)
    Just aid | aid == tickId root -> return (-1)  -- explicit root is the same target as Nothing
             | otherwise           -> findPos "afterTickId" aid contentOrdered

  checkMoveOrder tid mAfter tidPos afterPos contentOrdered

  let resolvedAfter = maybe (tickId root) id mAfter

  ((newTid, innerMapping), outerMapping) <-
    if tidPos > afterPos
      then -- Backward: at tid (pop >>= \d -> at after (push d))
        sneakyAt @branch tid $ do
          d                  <- popTick @branch
          (newTid, innerMap) <- sneakyAt @branch resolvedAfter $ pushTick @branch d
          return (newTid, innerMap)
      else -- Forward: at after (at tid pop >>= push)
        sneakyAt @branch resolvedAfter $ do
          (d, innerMap) <- sneakyAt @branch tid $ popTick @branch
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
  movingTick <- case List.drop tidPos ordered of
    (t : _) -> return t
    []      -> fail "checkMoveOrder: tick position out of range"
  let newPos = afterPos + 1

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
    -- 'listFiles' returns each direct child's *full* path already (see the
    -- 'ListFiles' interpreter in Storyteller.Core.Git — it filters the working
    -- tree's full keys by 'isDirectChild', it does not strip the parent). So
    -- an entry is used as the path verbatim; re-joining it onto 'dir' would
    -- double the directory component (e.g. chapters/chapters/ch1.md).
    go dir acc = do
      entries <- listFiles @(BranchTag branch) dir
      foldM visit acc entries

    visit acc path = do
      isDir <- isDirectory @(BranchTag branch) path
      if isDir
        then go path acc
        else do
          content <- readFile @(BranchTag branch) path
          return (Map.insert path content acc)

-- ---------------------------------------------------------------------------
-- Working-tree commit
-- ---------------------------------------------------------------------------
--
-- Reconciles an arbitrary edited working tree against the committed atom
-- chain, conservatively: only trimming (removing some of an atom's own
-- original bytes, from its front and/or back — never its middle) can change
-- an atom's classification. Padding alone, with no trim, is indistinguishable
-- from an adjacent insertion and so is never attributed to an untouched atom.
--
--   * Untouched (no trim recovered) -> kept as-is: same tick id untouched.
--   * Trimmed, nonzero content remaining (after folding in any immediately
--     adjacent new bytes) -> changed: a same-position replacement tick,
--     same pattern as 'editAtom' (@at tid $ drop >> withFS (write; store)@).
--   * Trimmed to nothing -> dropped: same pattern as 'deleteTick'
--     (@at tid drop@).
--   * New content that isn't absorbed by an adjacent trimmed atom -> a
--     standalone new tick, inserted after whatever currently precedes it.
--
-- See Storyteller.CommitWorkingTreeSpec for the full contract and the
-- reasoning behind the fold rule.

-- | One committed atom's own contributed bytes, in the order they were
--   written — the file's history expressed at atom granularity rather than
--   as opaque length checkpoints.
type AtomHistory = [(TickId, BS.ByteString)]

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
commitWorkingTree = listFiles @project "/" >>= commitFiles @project @branch

-- | Reconcile only the given files' working-tree content against their atom
--   history, rather than every file in the branch — same rule as
--   'commitWorkingTree' ('commitFile' per existing file, 'storeNewFiles' for
--   ones with no history yet), just scoped to a caller-chosen subset. Used
--   directly where a command only ever touches specific paths (e.g. a
--   branch-level file upload) and reconciling unrelated files' pending
--   working-tree edits would be out of scope for that command.
commitFiles
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[ FileSystem      project
                , FileSystemRead  project
                , FileSystemWrite project
                , StoryBranch branch
                , StoryStorage
                , Fail ] r )
  => [FilePath] -> Sem r [(TickId, TickId)]
commitFiles files = do
  mapping <- foldM (commitFile @project @branch) Map.empty files
  newFiles <- filterM (fmap null . buildAtomHistory @project @branch) files
  storeNewFiles @project @branch newFiles
  let fullMapping = Map.toList mapping
  updateReferences fullMapping
  return fullMapping

-- | Commit one file's reconciliation, threading a running old->current tick
--   id remap table (needed because rebasing one atom's tail can move ids
--   for atoms processed later, in this file or — since 'at' rebases the
--   whole branch — any other file too).
commitFile
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[ FileSystem project, FileSystemRead project, FileSystemWrite project
                , StoryBranch branch, StoryStorage, Fail ] r )
  => Map TickId TickId -> FilePath -> Sem r (Map TickId TickId)
commitFile table file = do
  history <- buildAtomHistory @project @branch file
  if null history then return table else do
    target <- readWorking @project file
    root   <- rootTickId @branch
    let matches  = matchAtoms history target
        n        = length matches
        gaps     = gapContents matches target
        fates    = gapFates matches gaps
        contents = finalAtomContents matches target gaps fates
        outs     = classify matches contents
        -- 'gaps'/'fates' carry one entry per atom (the gap immediately
        -- before it) plus one trailing entry for the gap after the last
        -- atom; zip5 below pairs each atom with its own leading gap/fate,
        -- leaving that trailing pair for the final 'emitStandaloneGap' call.
        perAtom  = zip5 matches (take n gaps) (take n fates) contents outs
        (tailGap, tailFate) = case (List.drop n gaps, List.drop n fates) of
          (g : _, f : _) -> (g, f)
          _              -> (BS.empty, Standalone)
    (table1, anchor1) <- foldM (commitAtom @project @branch file) (table, root) perAtom
    fst <$> emitStandaloneGap @project @branch file table1 anchor1 tailGap tailFate

-- | Process the gap immediately before this atom (folding it in is handled
--   as part of the atom's own content — see 'finalAtomContents' — so only
--   a standalone gap needs its own tick here), then the atom itself: left
--   untouched if kept, replaced in place if changed, removed if dropped.
commitAtom
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[ FileSystem project, FileSystemRead project, FileSystemWrite project
                , StoryBranch branch, StoryStorage, Fail ] r )
  => FilePath
  -> (Map TickId TickId, TickId)
  -> (AtomMatch, BS.ByteString, GapFate, BS.ByteString, AtomOutcome)
  -> Sem r (Map TickId TickId, TickId)
commitAtom file (table, anchor) (m, gap, fate, content, outcome) = do
  (table1, anchor1) <- emitStandaloneGap @project @branch file table anchor gap fate
  let origId = resolveId table1 (amTickId m)
  case outcome of
    Kept -> return (table1, origId)
    Dropped -> do
      (_, tailMapping) <- sneakyAt @branch origId (drop @branch)
      return (composeMapping table1 tailMapping, anchor1)
    Changed -> do
      (newTid, tailMapping) <- sneakyAt @branch origId $ do
        drop @branch
        withFS @branch $ do
          appendFile @project file content
          storeAs @branch (Atom file (TE.decodeUtf8With TE.lenientDecode content))
      return (composeMapping table1 (tailMapping ++ [(origId, newTid)]), newTid)

-- | A gap that folded onto a neighbor was already absorbed into that atom's
--   own content by 'finalAtomContents' — nothing to do here. A standalone
--   gap becomes its own new tick, inserted right after @anchor@ (whatever
--   currently precedes it in the chain).
emitStandaloneGap
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[ FileSystem project, FileSystemRead project, FileSystemWrite project
                , StoryBranch branch, StoryStorage, Fail ] r )
  => FilePath -> Map TickId TickId -> TickId -> BS.ByteString -> GapFate -> Sem r (Map TickId TickId, TickId)
emitStandaloneGap file table anchor content fate
  | fate /= Standalone || BS.null content = return (table, anchor)
  | otherwise = do
      (newTid, tailMapping) <- sneakyAtWithFS @branch anchor $ do
        appendFile @project file content
        storeAs @branch (Atom file (TE.decodeUtf8With TE.lenientDecode content))
      return (composeMapping table tailMapping, newTid)

resolveId :: Map TickId TickId -> TickId -> TickId
resolveId table tid = Map.findWithDefault tid tid table

-- | Fold a freshly-returned old->new mapping (from one 'at' call's tail
--   rebase) into the running table: existing entries whose current id was
--   itself just remapped follow the new mapping, and brand new entries are
--   added for ids not previously tracked.
composeMapping :: Map TickId TickId -> [(TickId, TickId)] -> Map TickId TickId
composeMapping table new =
  let newMap       = Map.fromList new
      updatedOld   = Map.map (\cur -> Map.findWithDefault cur cur newMap) table
      freshEntries = Map.filterWithKey (\k _ -> not (Map.member k table)) newMap
  in Map.union updatedOld freshEntries

-- | The branch's root tick — the anchor used for content inserted before
--   the first atom of a file.
rootTickId :: forall branch r. Members '[StoryBranch branch, Fail] r => Sem r TickId
rootTickId = do
  chain <- follow @branch [] (\acc t -> (t : acc, tickParent t))
  case chain of
    (root : _) -> return (tickId root)
    []         -> fail "commitWorkingTree: branch has no root tick"

-- | A file's history expressed at atom granularity: each committed tick's
--   own contributed bytes, oldest-first.
buildAtomHistory
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[StoryBranch branch, FileSystemRead project, FileSystem project, Fail] r )
  => FilePath -> Sem r AtomHistory
buildAtomHistory file = do
  ticks     <- follow @branch [] $ \acc tick -> (tick : acc, tickParent tick)
  snapshots <- mapM (readSnapshotAt @project @branch) ticks
  return $ deriveAtomHistory file snapshots

deriveAtomHistory :: FilePath -> [(TickId, Map FilePath BS.ByteString)] -> AtomHistory
deriveAtomHistory file = go BS.empty
  where
    go _ [] = []
    go prevContent ((tid, m) : rest) =
      case Map.lookup file m of
        Nothing      -> go prevContent rest
        Just content -> (tid, BS.drop (BS.length prevContent) content) : go content rest

readSnapshotAt
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[StoryBranch branch, FileSystemRead project, FileSystem project, Fail] r )
  => Tick
  -> Sem r (TickId, Map FilePath BS.ByteString)
readSnapshotAt tick = do
  -- Recurse into subdirectories (via 'readAllFiles') rather than a flat
  -- 'listFiles "/"': that flat listing includes directory entries, and
  -- 'readFile' on a directory (e.g. @chapters@) fails with "is a directory".
  -- 'readAllFiles' walks the tree and returns only files, which is what an
  -- atom-history snapshot needs — anything nested under @chapters/@ included.
  fileMap <- readAtWithFS @branch (tickId tick) (readAllFiles @branch)
  return (tickId tick, fileMap)

-- ---------------------------------------------------------------------------
-- Matching: recover each atom's surviving core from the target content
-- ---------------------------------------------------------------------------

data AtomMatch = AtomMatch
  { amTickId      :: TickId
  , amOriginalLen :: Int
  , amCoreStart   :: Int  -- ^ offset into the target content
  , amCoreLen     :: Int  -- ^ 0 if the atom didn't survive at all
  }

data AtomOutcome = Kept | Changed | Dropped deriving Eq
data GapFate = FoldBack | FoldFront | Standalone deriving Eq

-- | Walk atoms oldest-first, anchoring each one's surviving core (a
--   contiguous, order-preserving substring of its original content) in the
--   target content via longest-common-substring search from the current
--   cursor onward. Trimming is only ever recognized at an atom's front
--   and/or back — a substring match is exactly that: no interior deletions.
matchAtoms :: AtomHistory -> BS.ByteString -> [AtomMatch]
matchAtoms history target = go 0 history
  where
    go _ [] = []
    go cursor ((tid, orig) : rest) =
      let remaining     = BS.drop cursor target
          (_, bOff, len) = longestCommonSubstring orig remaining
          coreStart     = cursor + bOff
          cursor'       = coreStart + len
      in AtomMatch tid (BS.length orig) coreStart len : go cursor' rest

isKept :: AtomMatch -> Bool
isKept m = amCoreLen m == amOriginalLen m

coreOf :: BS.ByteString -> AtomMatch -> BS.ByteString
coreOf target m = BS.take (amCoreLen m) (BS.drop (amCoreStart m) target)

-- | The N+1 stretches of target content between (and around) atom cores.
gapContents :: [AtomMatch] -> BS.ByteString -> [BS.ByteString]
gapContents matches target = go 0 matches
  where
    go cursor [] = [BS.drop cursor target]
    go cursor (m : rest) =
      BS.take (amCoreStart m - cursor) (BS.drop cursor target)
        : go (amCoreStart m + amCoreLen m) rest

-- | Attribute each gap to a neighbor (folded) or mark it standalone: fold
--   onto the preceding atom's back if it's eligible, else the following
--   atom's front if eligible, else standalone. An atom is only eligible if
--   it's *partially* trimmed — a nonempty core remains, but not the whole
--   original. A fully-kept atom was never changing, so nothing folds onto
--   it; a fully-dropped atom leaves no surviving anchor to fold onto.
--   Each gap's fate depends only on its immediate neighbors' eligibility, so
--   this is a sliding window over (previous atom eligible?, gap, next atom
--   eligible?) rather than indexed lookups — 'gaps' has one more entry than
--   'matches' (a gap before each atom plus one trailing gap), so the window
--   edges are padded with 'False' on either side via zipWith3's own
--   length-matching (all three lists here are exactly @length matches + 1@).
gapFates :: [AtomMatch] -> [BS.ByteString] -> [GapFate]
gapFates matches gaps = zipWith3 fate3 gaps prevElig curElig
  where
    elig     = map (\m -> amCoreLen m > 0 && amCoreLen m < amOriginalLen m) matches
    prevElig = False : elig
    curElig  = elig ++ [False]
    fate3 gap prev cur
      | BS.null gap = Standalone
      | prev        = FoldBack
      | cur         = FoldFront
      | otherwise   = Standalone

-- | Each atom's final content: its surviving core plus whatever gaps
--   folded onto its front\/back. 'gaps'/'fates' each carry one leading
--   entry per atom plus a trailing one; zip5 pairs each atom with its own
--   leading (front) and following (back) gap/fate instead of indexing.
finalAtomContents :: [AtomMatch] -> BS.ByteString -> [BS.ByteString] -> [GapFate] -> [BS.ByteString]
finalAtomContents matches target gaps fates =
  [ frontFold ff fg <> coreOf target m <> backFold bf bg
  | (m, ff, fg, bf, bg) <- zip5 matches frontFates frontGaps backFates backGaps ]
  where
    n          = length matches
    frontFates = take n fates
    frontGaps  = take n gaps
    backFates  = List.drop 1 fates
    backGaps   = List.drop 1 gaps
    frontFold fate gap = if fate == FoldFront then gap else BS.empty
    backFold  fate gap = if fate == FoldBack  then gap else BS.empty

-- | Classify each atom given its already-computed final content (core plus
--   any folded-in gap bytes — see 'finalAtomContents').
classify :: [AtomMatch] -> [BS.ByteString] -> [AtomOutcome]
classify matches contents =
  [ if isKept m then Kept else if BS.null fc then Dropped else Changed
  | (m, fc) <- zip matches contents ]

-- | Longest common substring of two byte strings: returns the offset into
--   each and the shared length. @(0, 0, 0)@ if either is empty or there is
--   no overlap. O(n*m) time and space — fine for atom-sized inputs; this
--   is the one place that would need revisiting for very large files.
longestCommonSubstring :: BS.ByteString -> BS.ByteString -> (Int, Int, Int)
longestCommonSubstring a b
  | n == 0 || m == 0 = (0, 0, 0)
  | otherwise        = (bestI - best, bestJ - best, best)
  where
    n = BS.length a
    m = BS.length b
    dp :: Array (Int, Int) Int
    dp = listArray ((0, 0), (n, m)) [ cellAt i j | i <- [0 .. n], j <- [0 .. m] ]
    cellAt 0 _ = 0
    cellAt _ 0 = 0
    cellAt i j
      | BS.index a (i - 1) == BS.index b (j - 1) = dp ! (i - 1, j - 1) + 1
      | otherwise                                 = 0
    (best, bestI, bestJ) =
      List.foldl' pick (0, 0, 0) [ (dp ! (i, j), i, j) | i <- [1 .. n], j <- [1 .. m] ]
    pick acc@(bl, _, _) cand@(l, _, _) = if l > bl then cand else acc

-- | New files present in the working tree but absent from history: no
--   reconciliation needed, just a plain commit. Reads their target content
--   before resetting (which discards the pending buffer for files already
--   reconciled via 'commitFile') so only the new files' bytes get replayed
--   onto the now-current head.
storeNewFiles
  :: forall project branch r
  .  ( project ~ BranchTag branch
     , Members '[ FileSystem project, FileSystemRead project, FileSystemWrite project
                , StoryBranch branch, StoryStorage, Fail ] r )
  => [FilePath] -> Sem r ()
storeNewFiles [] = return ()
storeNewFiles files = do
  contents <- mapM (\f -> (f,) <$> readWorking @project f) files
  reset @branch
  mapM_ (\(f, c) -> writeFile @project f c) contents
  _ <- store @branch "add new files"
  return ()

readWorking
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => FilePath -> Sem r BS.ByteString
readWorking path = do
  exists <- fileExists @project path
  if exists then readFile @project path else return BS.empty
