-- | libgit2 FFI marshalling layer: converts between plain Haskell values
-- (hex 'Text' hashes, 'ByteString' content) and libgit2's C API, throwing
-- a descriptive 'IOError' (via 'Control.Exception.throwIO') on any
-- non-zero libgit2 return code. Deliberately plain 'IO', not
-- 'Polysemy.Sem' -- mirrors 'Runix.Git.Store'/'Runix.Git.Batch''s own
-- style, so 'Runix.Git.runGitFFIPerCall' can wrap these the same way
-- 'Runix.Git.embedBatch' already wraps those.
module Runix.Git.FFI
  ( libgit2Init
  , RepoHandle
  , openRepository
  , closeRepository
  , withRepository
  , resolveRef
  , readObjectRaw
  , writeObjectRaw
  , readBlob
  , writeBlob
  , createOrUpdateRef
  , deleteRef
  , listRefsGlob
  , isDescendantOfOrEqual
  , isAncestorOfAny
  ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import Control.Exception (bracket, throwIO)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Storable (peek, peekByteOff)
import System.IO.Unsafe (unsafePerformIO)

import Runix.Git.FFI.Raw
import Runix.Git.FFI.Types
  ( Repository, Odb, GitOid, oidSize, oidToBytes, oidFromBytes )
import Runix.Git.Hash (ObjectKind(..))

-- | Opaque handle to an open repository -- callers outside this module
-- never see the underlying @Ptr Repository@, so nothing above the FFI
-- marshalling layer needs to import "Foreign.Ptr" or know this is backed
-- by a C pointer at all.
--
-- Bundles the repository's @git_odb*@, opened once at 'openRepository'
-- time and reused for every subsequent read\/write, rather than
-- reopening it (@git_repository_odb@) and tearing it back down
-- (@git_odb_free@) around each individual object access the way an
-- earlier version of this module did. That earlier shape was a real,
-- measured regression against the CLI interpreter it replaced (its
-- persistent @git cat-file --batch@ reader pays this cost exactly once
-- per process, not once per object): @git_odb_free@ dropping the odb's
-- refcount to zero tears down its backends, so the next call has to
-- redo backend setup (re-scanning loose\/pack directories) instead of
-- reusing what's already open -- paid on literally every 'ReadObject'\/
-- 'WriteObject'\/'ReadCommit'\/'WriteCommit'.
data RepoHandle = RepoHandle (Ptr Repository) (Ptr Odb)

-- | libgit2 requires a one-time, process-global initialization before any
-- other call. Guarded so every 'withRepository' can call this
-- unconditionally without callers needing to reason about ordering across
-- a process that may open many repositories (e.g. every spec in a test
-- suite).
{-# NOINLINE initGuard #-}
initGuard :: MVar Bool
initGuard = unsafePerformIO (newMVar False)

libgit2Init :: IO ()
libgit2Init = modifyMVar_ initGuard $ \initialized ->
  if initialized
    then return initialized
    else do
      -- Unlike every other libgit2 call here, success is a positive count
      -- of active initializations (not 0) -- only a negative return is an
      -- error. See @vendor/libgit2/include/git2/global.h@.
      rc <- c_git_libgit2_init
      unless (rc >= 0) (throwLastError "git_libgit2_init" rc)
      return True

-- | Open the repository at @path@, creating a bare repository there first
-- if none exists yet -- the FFI-native, subprocess-free equivalent of
-- 'Runix.Git.ensureRepoExists'. Frees the repository handle when the
-- action finishes. 'openRepository'/'closeRepository' are exposed
-- separately for 'Runix.Git.runGitFFIPerCall', which needs a
-- 'Polysemy.Resource.bracket' (a 'Sem'-level bracket, not this plain-'IO'
-- one) around the open handle.
withRepository :: FilePath -> (RepoHandle -> IO a) -> IO a
withRepository path = bracket (openRepository path) closeRepository

closeRepository :: RepoHandle -> IO ()
closeRepository (RepoHandle repo odb) = do
  c_git_odb_free odb
  c_git_repository_free repo

openRepository :: FilePath -> IO RepoHandle
openRepository path = do
  libgit2Init
  repo <-
    withCString path $ \pathPtr ->
      alloca $ \repoPtrPtr -> do
        rc <- c_git_repository_open repoPtrPtr pathPtr
        if rc == 0
          then peek repoPtrPtr
          else do
            rc2 <- c_git_repository_init repoPtrPtr pathPtr 1 {- bare -}
            if rc2 == 0
              then peek repoPtrPtr
              else throwLastError "git_repository_init" rc2
  odb <- alloca $ \odbPtrPtr -> do
    checkCall "git_repository_odb" =<< c_git_repository_odb odbPtrPtr repo
    peek odbPtrPtr
  return (RepoHandle repo odb)

-- | 'Nothing' if the ref doesn't exist (@GIT_ENOTFOUND@), matching
-- 'Runix.Git.ResolveRef's semantics.
resolveRef :: RepoHandle -> Text -> IO (Maybe Text)
resolveRef (RepoHandle repo _) name =
  allocaBytes oidSize $ \oidPtr ->
    withCString (T.unpack name) $ \namePtr -> do
      rc <- c_git_reference_name_to_id oidPtr repo namePtr
      if rc == gitENOTFOUND
        then return Nothing
        else do
          checkCall "git_reference_name_to_id" rc
          Just . oidToBytes <$> BS.packCStringLen (castPtr oidPtr, oidSize)

-- | Reads any object's raw content by hash, along with its kind -- the
-- generic form 'readBlob' and the FFI interpreter's tree/commit reads are
-- both built on.
readObjectRaw :: RepoHandle -> Text -> IO (ObjectKind, ByteString)
readObjectRaw (RepoHandle _ odb) hex =
  withOid hex $ \oidPtr ->
    alloca $ \objPtrPtr -> do
      checkCall "git_odb_read" =<< c_git_odb_read objPtrPtr odb oidPtr
      objPtr <- peek objPtrPtr
      typ <- c_git_odb_object_type objPtr
      kind <- case fromGitObjectT typ of
        Just k  -> return k
        Nothing -> throwIO (userError ("Runix.Git.FFI.readObjectRaw: unsupported object type " <> show typ))
      dataPtr <- c_git_odb_object_data objPtr
      size <- c_git_odb_object_size objPtr
      content <- BS.packCStringLen (castPtr dataPtr, fromIntegral size)
      c_git_odb_object_free objPtr
      return (kind, content)

-- | Writes @content@ as an object of the given kind, returning its hash.
-- libgit2 computes the hash itself here (unlike 'Runix.Git.Store', which
-- precomputes it to avoid a round trip through the CLI) -- see
-- 'GitFFISpec' for the check that it agrees with
-- 'Runix.Git.Hash.hashObject' on the same content.
writeObjectRaw :: RepoHandle -> ObjectKind -> ByteString -> IO Text
writeObjectRaw (RepoHandle _ odb) kind content =
  allocaBytes oidSize $ \oidPtr ->
    BS.useAsCStringLen content $ \(dataPtr, len) -> do
      checkCall "git_odb_write"
        =<< c_git_odb_write oidPtr odb (castPtr dataPtr) (fromIntegral len) (toGitObjectT kind)
      oidToBytes <$> BS.packCStringLen (castPtr oidPtr, oidSize)

-- | Reads a blob's raw content by hash. Throws if the hash isn't found or
-- doesn't name a blob.
readBlob :: RepoHandle -> Text -> IO ByteString
readBlob repo hex = do
  (kind, content) <- readObjectRaw repo hex
  case kind of
    Blob -> return content
    _    -> throwIO (userError ("Runix.Git.FFI.readBlob: not a blob: " <> T.unpack hex))

writeBlob :: RepoHandle -> ByteString -> IO Text
writeBlob repo = writeObjectRaw repo Blob

-- | Direct (non-symbolic) ref create-or-update -- @force=1@ makes this
-- cover both 'Runix.Git.CreateRef' and 'Runix.Git.UpdateRef'. No odb
-- refresh needed: writes go through 'writeObjectRaw' (@git_odb_write@,
-- same cached odb handle this reads from), not a filesystem write
-- bypassing libgit2, so there's nothing for this handle's odb to be
-- stale about.
createOrUpdateRef :: RepoHandle -> Text -> Text -> IO ()
createOrUpdateRef (RepoHandle repo _) name hex =
  withOid hex $ \oidPtr ->
    withCString (T.unpack name) $ \namePtr ->
      alloca $ \refPtrPtr -> do
        checkCall "git_reference_create"
          =<< c_git_reference_create refPtrPtr repo namePtr oidPtr 1 {- force -} nullPtr
        refPtr <- peek refPtrPtr
        c_git_reference_free refPtr

deleteRef :: RepoHandle -> Text -> IO ()
deleteRef (RepoHandle repo _) name =
  withCString (T.unpack name) $ \namePtr ->
    alloca $ \refPtrPtr -> do
      checkCall "git_reference_lookup" =<< c_git_reference_lookup refPtrPtr repo namePtr
      refPtr <- peek refPtrPtr
      checkCall "git_reference_delete" =<< c_git_reference_delete refPtr
      c_git_reference_free refPtr

-- | All direct (non-symbolic) refs whose name starts with @prefix@,
-- matching 'Runix.Git.ListRefs's semantics -- symbolic refs (e.g. @HEAD@,
-- if it were ever matched by @prefix@) have no direct target oid and are
-- silently skipped, exactly as they'd never appear in a @for-each-ref@
-- listing of direct refs either.
listRefsGlob :: RepoHandle -> Text -> IO [(Text, Text)]
listRefsGlob (RepoHandle repo _) prefix =
  withCString (T.unpack prefix <> "*") $ \globPtr ->
    alloca $ \iterPtrPtr -> do
      checkCall "git_reference_iterator_glob_new"
        =<< c_git_reference_iterator_glob_new iterPtrPtr repo globPtr
      iter <- peek iterPtrPtr
      result <- collect iter
      c_git_reference_iterator_free iter
      return result
  where
    collect iter =
      alloca $ \refPtrPtr -> do
        rc <- c_git_reference_next refPtrPtr iter
        if rc == gitITEROVER
          then return []
          else do
            checkCall "git_reference_next" rc
            refPtr <- peek refPtrPtr
            namePtr <- c_git_reference_name refPtr
            name <- T.pack <$> peekCString namePtr
            oidPtr <- c_git_reference_target refPtr
            entry <-
              if oidPtr == nullPtr
                then return Nothing
                else Just . (,) name . oidToBytes <$> BS.packCStringLen (castPtr oidPtr, oidSize)
            c_git_reference_free refPtr
            rest <- collect iter
            return (maybe rest (: rest) entry)

-- | @True@ if @commit@ either equals @ancestor@ or descends from it --
-- 'Runix.Git.IsAncestorOfAny's per-target check, since
-- @git_graph_descendant_of@ alone (unlike @git rev-list@, which the CLI
-- interpreter's version naturally includes the head commit in) never
-- considers a commit its own descendant.
isDescendantOfOrEqual :: RepoHandle -> Text -> Text -> IO Bool
isDescendantOfOrEqual (RepoHandle repo _) commitHex ancestorHex
  | commitHex == ancestorHex = return True
  | otherwise =
      withOid commitHex $ \commitOid ->
        withOid ancestorHex $ \ancestorOid -> do
          rc <- c_git_graph_descendant_of repo commitOid ancestorOid
          if rc == 0 || rc == 1
            then return (rc == 1)
            else throwLastError "git_graph_descendant_of" rc

-- | @True@ if any of @targets@ is an ancestor of (or equal to) @headHex@
-- -- 'Runix.Git.IsAncestorOfAny's full check, short-circuiting on the
-- first hit rather than computing every target's answer.
isAncestorOfAny :: RepoHandle -> [Text] -> Text -> IO Bool
isAncestorOfAny _ [] _ = return False
isAncestorOfAny repo (t : ts) headHex = do
  hit <- isDescendantOfOrEqual repo headHex t
  if hit then return True else isAncestorOfAny repo ts headHex

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

withOid :: Text -> (Ptr GitOid -> IO a) -> IO a
withOid hex action = case oidFromBytes hex of
  Left err -> throwIO (userError ("Runix.Git.FFI: malformed object hash " <> show hex <> ": " <> err))
  Right bytes ->
    allocaBytes oidSize $ \oidPtr -> do
      BS.useAsCStringLen bytes $ \(src, len) -> copyBytes (castPtr oidPtr) src len
      action oidPtr

-- | @git_object_t@ values (see @vendor/libgit2/include/git2/types.h@).
toGitObjectT :: ObjectKind -> CInt
toGitObjectT Commit = 1
toGitObjectT Tree   = 2
toGitObjectT Blob   = 3

fromGitObjectT :: CInt -> Maybe ObjectKind
fromGitObjectT 1 = Just Commit
fromGitObjectT 2 = Just Tree
fromGitObjectT 3 = Just Blob
fromGitObjectT _ = Nothing

-- | @GIT_ENOTFOUND@ (see @vendor/libgit2/include/git2/errors.h@).
gitENOTFOUND :: CInt
gitENOTFOUND = -3

-- | @GIT_ITEROVER@ (see @vendor/libgit2/include/git2/errors.h@).
gitITEROVER :: CInt
gitITEROVER = -31

checkCall :: String -> CInt -> IO ()
checkCall ctx rc = unless (rc == 0) (throwLastError ctx rc)

throwLastError :: String -> CInt -> IO a
throwLastError ctx rc = do
  errPtr <- c_git_error_last
  msg <-
    if errPtr == nullPtr
      then return "(no libgit2 error available)"
      else do
        msgPtr <- peekByteOff errPtr 0 :: IO CString
        if msgPtr == nullPtr then return "(no message)" else peekCString msgPtr
  throwIO (userError (ctx <> " failed (code " <> show rc <> "): " <> msg))
