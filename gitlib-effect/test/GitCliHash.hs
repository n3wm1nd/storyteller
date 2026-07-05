{-# LANGUAGE OverloadedStrings #-}

-- | The implementation 'Runix.Git.Hash.hashObject' replaces: shell out to
-- a real @git@ subprocess and ask it for an object's hash. Kept here (not
-- in the library) purely as the oracle for @GitHashSpec@'s property tests
-- and the baseline for @HashBench@'s throughput comparison.
--
-- Never touches a repository: @git hash-object@ without @-w@ needs none.
-- @--literally@ skips git's own format validation (which otherwise
-- rejects e.g. non-well-formed tree/commit content), since hashing itself
-- doesn't depend on the content being well-formed -- only writing it does.
module GitCliHash (gitHashObject) where

import Control.Concurrent (forkIO)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as TE
import System.IO (hClose, hSetBinaryMode)
import System.Process

import Runix.Git (ObjectHash(..))
import Runix.Git.Hash (ObjectKind, objectKindTag)

gitHashObject :: ObjectKind -> ByteString -> IO ObjectHash
gitHashObject kind content = do
  (Just hin, Just hout, _, ph) <- createProcess (proc "git"
        [ "hash-object", "-t", BS8.unpack (objectKindTag kind)
        , "--stdin", "--literally" ])
    { std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
  hSetBinaryMode hin True
  hSetBinaryMode hout True
  -- Written on a separate thread so a large @content@ can't deadlock
  -- against git's own stdout buffer filling up before we start reading it.
  _ <- forkIO (BS.hPut hin content >> hClose hin)
  out <- BS.hGetContents hout
  _ <- waitForProcess ph
  return (ObjectHash (TE.decodeUtf8 (BS.takeWhile (/= 10) out)))
