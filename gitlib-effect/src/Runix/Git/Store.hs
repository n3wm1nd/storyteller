{-# LANGUAGE OverloadedStrings #-}

-- | Direct, in-process loose-object writes — the write-side counterpart
-- to "Runix.Git.Batch"'s batched reads.
--
-- 'Runix.Git.runGitIO' used to learn each written object's hash by
-- shelling out to @git hash-object@\/@git mktree@ per call — one @git@
-- subprocess spawn per object written. Since the hash and the exact bytes
-- git would store for that content are both already fully determined by
-- the content itself ('Runix.Git.Hash.hashObject'), this writes the loose
-- object straight to @\<gitdir\>\/objects\/\<aa\>\/\<bbbb...\>@ instead —
-- no subprocess at all, for any object kind.
--
-- This is what actually dominated wall-clock time for a deep tail-replay
-- (a `moveTick`\/`mergeAtoms`\/`splitTick`\/`editAtom` far back in
-- history): the interpretH tax the storage-monad migration
-- (PLAN-storage-monad.md) removed was real, but production timing showed
-- no improvement, because the real cost was ~3-4ms of process-spawn
-- overhead multiplied by the write count of a deep rebase (further
-- multiplied by however many other branches' histories reference the
-- rewritten range) — thousands of separate @git@ invocations, each
-- independent of any Polysemy concern. Removing the subprocess entirely
-- for writes is the fix for that: see @bench/RealGitPerf.hs@ for the
-- reproduction against a real repository.
--
-- See @GitStoreSpec@ (gitlib-effect-test) for the correctness check
-- against real git (an object written here round-trips through
-- @git cat-file@\/@git fsck@ exactly as if @git hash-object -w@ had
-- written it).
module Runix.Git.Store
  ( writeLooseObject
  ) where

import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath ((</>))
import System.IO (openBinaryTempFile, hClose)

import Runix.Git.Hash (ObjectKind, objectKindTag, hashObject)

-- | Write @content@ as a loose object of the given kind directly into the
--   repository at @gitDir@ (the actual @.git@ directory — see
--   'Runix.Git.resolveGitDir' — not the worktree root, so this works for
--   bare repos and worktrees too). Returns the object's hash, computed
--   the same way 'Runix.Git.Hash.hashObject' does.
--
--   A no-op (beyond the existence check) if the object is already
--   present: content is content-addressed, so an existing object at that
--   hash is always byte-identical to what we'd write — writing it again
--   would be redundant work, not a correction.
--
--   Written via a temp file in the same target directory, then
--   'renameFile'd into place — atomic on the same filesystem, matching
--   git's own loose-object write discipline (never leaves a partially-
--   written object visible at the final path).
writeLooseObject :: FilePath -> ObjectKind -> ByteString -> IO Text
writeLooseObject gitDir kind content = do
  let hash         = hashObject kind content
      preimage     = objectKindTag kind <> " " <> BS8.pack (show (BS.length content)) <> "\0" <> content
      (dir2, rest) = T.splitAt 2 hash
      objDir       = gitDir </> "objects" </> T.unpack dir2
      objPath      = objDir </> T.unpack rest
  exists <- doesFileExist objPath
  if exists
    then return hash
    else do
      createDirectoryIfMissing True objDir
      (tmpPath, h) <- openBinaryTempFile objDir "obj.tmp"
      BSL.hPut h (Zlib.compress (BSL.fromStrict preimage))
      hClose h
      renameFile tmpPath objPath
      return hash
