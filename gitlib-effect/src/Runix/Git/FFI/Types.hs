-- | Opaque libgit2 handle types (phantom markers for 'Foreign.Ptr.Ptr',
-- never constructed on the Haskell side) and the fixed-size @git_oid@
-- encode/decode used to bridge 'Runix.Git.ObjectHash's hex 'Data.Text.Text'
-- and libgit2's raw 20-byte object ids (this build has
-- @GIT_EXPERIMENTAL_SHA256@ off, so @git_oid@ is exactly
-- @unsigned char id[20]@ -- see @vendor/libgit2/include/git2/oid.h@).
module Runix.Git.FFI.Types
  ( Repository
  , Odb
  , OdbObject
  , Reference
  , RefIterator
  , GitOid
  , GitError
  , oidSize
  , oidToBytes
  , oidFromBytes
  ) where

import qualified Data.ByteArray.Encoding as BA
import Data.ByteString (ByteString)
import qualified Data.Text.Encoding as TE
import Data.Text (Text)

data Repository
data Odb
data OdbObject
data Reference
data RefIterator
data GitOid
data GitError

-- | Byte width of a @git_oid@ in this (SHA1-only) build.
oidSize :: Int
oidSize = 20

-- | Render 20 raw id bytes the way 'Runix.Git.ObjectHash' stores them
-- (lowercase 40-char hex).
oidToBytes :: ByteString -> Text
oidToBytes = TE.decodeUtf8 . BA.convertToBase BA.Base16

-- | Parse an 'Runix.Git.ObjectHash's hex text back into the 20 raw bytes
-- libgit2's @git_oid@ expects. 'Left' on malformed hex (wrong length or
-- non-hex characters) -- both '"caller passed garbage"' bugs, not runtime
-- conditions this ever expects to hit in practice.
oidFromBytes :: Text -> Either String ByteString
oidFromBytes hex = BA.convertFromBase BA.Base16 (TE.encodeUtf8 hex)
