{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Composition for the @\/library\/{name}@ connection: the writer-facing
-- organizational view over one branch (books\/chapters\/scenes rather than
-- raw files) — see WS-PROTOCOL.md. Writer-specific in the same way
-- 'Server.Writer.Character' is: it knows the marker-word prose-detection
-- convention from WRITER.md, which 'Server.Core.Branch' has no business
-- knowing about.
--
-- Structure and per-file classification are pure (see
-- 'Storyteller.Writer.Library'); this module adds the three things that
-- can't be: the branch's current file list ('listAllFiles'), each
-- chapter's own heading, and each leaf's binary\/tracked flag. Both of the
-- latter are folded incrementally over the tick chain in one pass via
-- 'Storage.Ops.memoFold' ('LibraryFoldCache') rather than re-derived from
-- scratch on every push — a chapter's whole content lives directly in its
-- own commit message (see 'Storage.Core.readTick'), so the fold needs no
-- filesystem access at all, just commit reads, and "has this path ever had
-- an atom" is exactly the kind of monotonic fact ('Storage.Ops.hasAnyAtom'
-- would otherwise re-derive by walking to root every time, worst-case for
-- precisely the binary files it's checking for) a running accumulator
-- answers by construction: once a path's own atom is seen, it stays seen.
-- 'memoFold' means only the ticks that actually landed since the last push
-- get looked at for either question, not every chapter or every leaf,
-- every time. See WS-PROTOCOL.md's "stays push-cheap indefinitely" test —
-- this is what actually keeps a library tree covering many chapters (and
-- assets) cheap to re-push, not just narrowing the payload to one line per
-- chapter.
module Server.Writer.Library
  ( LibraryFoldCache(..)
  , libraryTree
  , chapterCreate
  ) where

import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text as T
import Polysemy (Sem)
import Runix.FileSystem (listAllFiles)

import Server.Core.Branch (Main, BranchOpen)
import Storyteller.Core.Git (BranchTag, runStorage)
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Writer.Library
  (LibraryNode(..), LibraryKind(..), UnitInfo, buildLibraryTree, narrativeUnits, classifyPath)

-- | The 'memoFold' accumulator: every chapter path's currently-known full
--   content, keyed by path, plus every path that has ever carried at
--   least one 'Ops.Atom' tick anywhere in history.
--
--   Full content, not just the first line: a plain sequential fold can't
--   tell in advance whether a later tick will turn out to be the file's
--   first line (an edit that replaces its very first atom, however rare),
--   so it tracks the same thing 'Storage.Ops.atomHistory' would
--   reconstruct from scratch — 'firstLine' is only taken at the point a
--   heading is actually needed.
--
--   The tracked-path set only ever grows: 'Storage.Ops.hasAnyAtom's own
--   "once atom, always atom" invariant means a path already in it can
--   never need to leave, so accumulating it here — rather than asking
--   'hasAnyAtom' fresh on every push — is exact, not an approximation.
data LibraryFoldCache = LibraryFoldCache
  { lfcChapters :: Map FilePath T.Text
  , lfcTracked  :: Set FilePath
  }

emptyLibraryFoldCache :: LibraryFoldCache
emptyLibraryFoldCache = LibraryFoldCache Map.empty Set.empty

-- | The memoFold step: any atom marks its own path as tracked; one whose
--   path also classifies as a chapter additionally folds its content in.
--   Everything else (a 'NonAtom', or an atom for some other kind of path)
--   leaves the relevant half of the cache untouched. No 'Storage.Ops.StoreT'
--   capability is actually exercised here — an atom's content lives in its
--   own commit message (see 'Storage.Core.readTick'), not a separate blob
--   'Storage.Core.readFile' would have to fetch — but the step still has
--   to be monadic in 'StoreT' to satisfy 'Ops.memoFold's own signature.
foldLibraryState :: Ops.StoreM m => LibraryFoldCache -> Ops.ObjectHash -> Ops.Tick -> Ops.StoreT m LibraryFoldCache
foldLibraryState acc _h tick = return $ case tick of
  Ops.Atom _ path _ content ->
    let tracked'  = Set.insert path (lfcTracked acc)
        chapters' = case classifyPath path of
          Unit -> Map.insertWith (flip (<>)) path content (lfcChapters acc)
          _    -> lfcChapters acc
    in acc { lfcTracked = tracked', lfcChapters = chapters' }
  _ -> acc

-- | The full organizational tree for this branch, plus every prose unit
--   already paired with its own beat sheet if any (see
--   'Storyteller.Writer.Library.narrativeUnits') — computed here, once, so
--   nothing downstream (this connection's push, and the Summarizer agent)
--   has to re-derive "which unit does this belong to" independently. Takes
--   and returns a 'LibraryFoldCache' — the memoized fold's own checkpoint
--   set, to be threaded straight through into the next call (see
--   'Server.Writer.Library.Connection', which persists it across repeated
--   ref-move pushes the same way it already threads its own file-set
--   accumulator).
libraryTree
  :: BranchOpen r
  => [(Ops.ObjectHash, LibraryFoldCache)]
  -> Sem r ([LibraryNode], [UnitInfo], [(Ops.ObjectHash, LibraryFoldCache)])
libraryTree cache = do
  paths <- listAllFiles @(BranchTag Main) "/"
  (folded, nextCache) <- runStorage @Main (Ops.memoFold foldLibraryState emptyLibraryFoldCache cache)
  let tree = withBinaryFlags (lfcTracked folded) (withHeadings (lfcChapters folded) (buildLibraryTree paths))
  return (tree, narrativeUnits tree, nextCache)

-- | Fill in 'lnHeading' for unit nodes (and recurse into folders) from
--   the already-folded content cache — a pure lookup, no filesystem or
--   'StoreT' access needed at this point.
withHeadings :: Map FilePath T.Text -> [LibraryNode] -> [LibraryNode]
withHeadings content = map go
  where
    go n = case lnKind n of
      Unit   -> n { lnHeading = Map.lookup (lnPath n) content >>= firstLine }
      Folder -> n { lnChildren = withHeadings content (lnChildren n) }
      _      -> n

-- | Fill in 'lnBinary' for every leaf (and recurse into folders) -- a
--   path has never had an atom if and only if it opted out of atom
--   tracking entirely (see "Storage.Ops"'s 'Storage.Ops.hasAnyAtom' and
--   the 'Binary'\/'Opaque' tick kinds it's checking for), which is exactly
--   the client's own cue not to open a prose\/atom viewer on it. A pure
--   lookup against the already-folded tracked-path set -- no per-leaf
--   'StoreT' walk needed at all, unlike the 'hasAnyAtom'-per-leaf shape
--   this replaced.
withBinaryFlags :: Set FilePath -> [LibraryNode] -> [LibraryNode]
withBinaryFlags tracked = map go
  where
    go n = case lnKind n of
      Folder -> n { lnChildren = withBinaryFlags tracked (lnChildren n) }
      _      -> n { lnBinary = not (Set.member (lnPath n) tracked) }

firstLine :: T.Text -> Maybe T.Text
firstLine t = case T.lines t of
  (l : _) | not (T.null l) -> Just l
  _                        -> Nothing

-- | Introduce @path@ as a new chapter file, seeded with its heading — the
--   one thing this connection's own creation command does that generic
--   'Server.Core.File.createFile' doesn't: write @# {name}@ as the file's
--   first line, the same "first H1 line is the display name" convention
--   'sheet.md' already uses (see WRITER.md). Fails on a path that already
--   has ticks, same as 'Server.Core.File.createFile'. Deliberately doesn't
--   validate that @path@ contains a marker word — library detection is
--   freeform (see 'Storyteller.Writer.Library'), so a path that doesn't
--   match is still created, just not later recognized as a 'Unit' node.
chapterCreate :: BranchOpen r => FilePath -> T.Text -> Sem r ()
chapterCreate path name = do
  existing <- runStorage @Main (Tick.fileTicksOf path)
  case existing of
    [] -> void (runStorage @Main (Ops.addAtom path ("# " <> name <> "\n")))
    _  -> fail ("chapterCreate: already exists: " <> path)
