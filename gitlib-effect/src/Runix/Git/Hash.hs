{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pure, in-process computation of git object hashes.
--
-- Git's object hash is a deterministic function of an object's type tag,
-- byte length, and content: @sha1(\<type\> " " \<len\> "\\0" \<content\>)@.
-- 'runGitIO' currently learns this hash by shelling out to @git
-- hash-object@/@mktree@ and parsing its stdout -- a full subprocess
-- round-trip purely to be told a value that's already fully determined by
-- content this module already holds. 'hashObject' computes it directly.
--
-- See @GitHashSpec@ (gitlib-effect-test) for property tests checking this
-- matches @git hash-object@ byte-for-byte, and @HashBench@
-- (gitlib-effect-hash-bench) for the throughput comparison.
module Runix.Git.Hash
  ( ObjectKind(..)
  , objectKindTag
  , hashObject
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import Crypto.Hash (hashWith, Digest)
import Crypto.Hash.Algorithms (SHA1(..))
import Data.ByteArray.Encoding (convertToBase, Base(Base16))

-- | The object types git computes hashes for. Trees and commits are
-- hashed the same way as blobs -- only the type tag in the preimage
-- differs -- so one function covers all three.
--
-- Returns the raw hex digest rather than 'Runix.Git.ObjectHash' so this
-- module has no dependency on 'Runix.Git' (which itself depends on this
-- module for its 'WriteCommit'/'WriteObject' interpreter) -- callers wrap
-- the result themselves.
data ObjectKind = Blob | Tree | Commit
  deriving (Show, Eq)

objectKindTag :: ObjectKind -> ByteString
objectKindTag Blob   = "blob"
objectKindTag Tree   = "tree"
objectKindTag Commit = "commit"

-- | Compute the git object hash for @content@, matching what
-- @git hash-object -t \<kind\> --stdin@ would report -- without spawning it.
hashObject :: ObjectKind -> ByteString -> Text
hashObject kind content =
  let header = objectKindTag kind <> " " <> BS8.pack (show (BS.length content)) <> "\0"
      digest = hashWith SHA1 (header <> content) :: Digest SHA1
  in TE.decodeUtf8 (convertToBase Base16 digest)
