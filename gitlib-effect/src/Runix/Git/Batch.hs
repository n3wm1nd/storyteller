{-# LANGUAGE OverloadedStrings #-}

-- | A persistent @git cat-file --batch@ process for reading objects.
--
-- Every read through 'Runix.Git.runGitIO' today ('ReadObject',
-- 'ReadCommit') forks a fresh @git@ subprocess per call. @cat-file
-- --batch@ lets one long-lived process serve many reads instead: write
-- one hash per line, read back exactly one response, repeat -- see the
-- BATCH OUTPUT section of @git-cat-file(1)@ for the wire format this
-- module parses:
--
-- @
--   present: \<oid\> SP \<type\> SP \<size\> LF \<contents, size bytes\> LF
--   missing: \<object\> SP missing LF
-- @
--
-- Output is flushed after every response by default (no @--buffer@), so
-- request/response pairs can be interleaved one at a time without
-- closing or restarting the process.
--
-- Wired into 'Runix.Git.runGitIO' via a caller-supplied access function
-- (@forall x. (BatchReader -> IO x) -> IO x@) rather than a hardcoded
-- lifetime -- see PLAN-git-storage-worker.md for why: a future
-- git-storage worker thread will be the sole owner and caller of a
-- 'BatchReader', so no locking/sharing primitive belongs in this module --
-- it stays exactly "one reader, opened, read from, closed." See
-- @GitBatchSpec@ (gitlib-effect-test) for the correctness check against
-- the existing write path, and @BatchBench@ (gitlib-effect-batch-bench)
-- for the throughput comparison against one-shot @git cat-file@ calls.
module Runix.Git.Batch
  ( BatchReader
  , openBatchReader
  , closeBatchReader
  , withBatchReader
  , readBatch
  ) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import System.IO
import System.Process

data BatchReader = BatchReader
  { brStdin  :: !Handle
  , brStdout :: !Handle
  , brProc   :: !ProcessHandle
  }

-- | Start a @git cat-file --batch@ process rooted at @repo@.
openBatchReader :: FilePath -> IO BatchReader
openBatchReader repo = do
  (Just hin, Just hout, _, ph) <- createProcess (proc "git" ["cat-file", "--batch"])
    { cwd = Just repo, std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
  hSetBinaryMode hin True
  hSetBinaryMode hout True
  return (BatchReader hin hout ph)

-- | Close stdin (signals EOF to git) and wait for the process to exit.
closeBatchReader :: BatchReader -> IO ()
closeBatchReader br = do
  hClose (brStdin br)
  _ <- waitForProcess (brProc br)
  return ()

withBatchReader :: FilePath -> (BatchReader -> IO a) -> IO a
withBatchReader repo = bracket (openBatchReader repo) closeBatchReader

-- | Read one object's type and raw content, or 'Nothing' if @hash@ is
-- missing or ambiguous. Must be called from a single thread at a time --
-- the protocol is strictly one request in flight per response.
--
-- Takes the hash as raw hex 'Text' rather than 'Runix.Git.ObjectHash' so
-- this module has no dependency on 'Runix.Git' (which depends on this
-- module to wire the reader into its interpreter) -- callers unwrap.
readBatch :: BatchReader -> Text -> IO (Maybe (Text, ByteString))
readBatch br h = do
  BS8.hPutStrLn (brStdin br) (TE.encodeUtf8 h)
  hFlush (brStdin br)
  headerLine <- BS8.hGetLine (brStdout br)
  case BS8.words headerLine of
    [_oid, "missing"]   -> return Nothing
    [_oid, "ambiguous"] -> return Nothing
    [_oid, typ, sizeStr] -> case BS8.readInt sizeStr of
      Just (size, _) -> do
        content <- BS.hGet (brStdout br) size
        _trailingNewline <- BS.hGet (brStdout br) 1
        return (Just (TE.decodeUtf8 typ, content))
      Nothing -> ioError (userError ("readBatch: bad size in header: " <> BS8.unpack headerLine))
    _ -> ioError (userError ("readBatch: unrecognised cat-file --batch header: " <> BS8.unpack headerLine))
