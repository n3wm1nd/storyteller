-- | libgit2 FFI marshalling layer: converts between plain Haskell values
-- (hex 'Text' hashes, 'ByteString' content) and libgit2's C API, throwing
-- a descriptive 'IOError' (via 'Control.Exception.throwIO') on any
-- non-zero libgit2 return code. Deliberately plain 'IO', not
-- 'Polysemy.Sem' -- mirrors 'Runix.Git.Store'/'Runix.Git.Batch''s own
-- style, so 'Runix.Git.runGitFFIPerCall' can wrap these the same way
-- 'Runix.Git.embedBatch' already wraps those.
module Runix.Git.FFI
  ( libgit2Init
  , FFIOptions(..)
  , defaultFFIOptions
  , libgit2DefaultFFIOptions
  , applyFFIOptions
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
import qualified Data.Set as Set
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
--
-- Writes go through @git_odb_write@ (see 'writeObjectRaw') rather than a
-- hand-rolled direct loose-object writer -- a real alternative was tried
-- (self-hashing, bypassing libgit2 entirely, mirroring
-- 'Runix.Git.Store') and measured no better once the actual dominant
-- cost ('isAncestorOfAny's old per-target @git_graph_descendant_of@
-- walk, see that function's own doc) was fixed; letting libgit2 own
-- writes end to end is simpler to reason about than maintaining a
-- second, hand-rolled object-writing path in parallel with it.
data RepoHandle = RepoHandle (Ptr Repository) (Ptr Odb)

-- | libgit2 requires a one-time, process-global initialization before any
-- other call. Guarded so every 'withRepository' can call this
-- unconditionally without callers needing to reason about ordering across
-- a process that may open many repositories (e.g. every spec in a test
-- suite).
{-# NOINLINE initGuard #-}
initGuard :: MVar Bool
initGuard = unsafePerformIO (newMVar False)

-- | Toggles for the two libgit2 strict-validation options this codebase
-- disables by default (see 'defaultFFIOptions') -- redundant integrity
-- checks for a content-addressed store that never takes an
-- externally-claimed hash on faith, but real, measured, opt-outable
-- costs (see 'applyFFIOptions's own doc for the mechanism each guards).
-- A plain record (not baked into 'libgit2Init' unconditionally) so a
-- benchmark can compare libgit2's own defaults against this codebase's
-- choice directly, instead of only ever being able to measure one side
-- (see gitlib-effect-stricthash-bench, which did this ad hoc with its
-- own local FFI import before this existed).
data FFIOptions = FFIOptions
  { ffiStrictHashVerification :: Bool
    -- ^ 'gitOptEnableStrictHashVerification'. libgit2 default: 'True'.
  , ffiStrictObjectCreation :: Bool
    -- ^ 'gitOptEnableStrictObjectCreation'. libgit2 default: 'True'.
  } deriving (Show, Eq)

-- | This codebase's production choice: both strict checks off. Every
-- object this module ever reads was named by a hash it (or an equally
-- trusted writer -- see 'Runix.Git.Store', the CLI interpreter's own
-- direct loose-object writer) computed from the content itself, and
-- every ref this module ever points somewhere targets a hash this
-- process itself just computed and wrote -- there is no
-- externally-claimed hash or target to validate in either case.
-- Disabling strict hash verification specifically was the actual root
-- cause of a live-production report that this FFI interpreter ran
-- slower than the CLI one it replaced despite winning every other
-- isolated measurement (a measured 5-8x read-throughput loss on
-- realistically-sized (hundreds of KB+) blobs -- see
-- gitlib-effect-stricthash-bench).
defaultFFIOptions :: FFIOptions
defaultFFIOptions = FFIOptions
  { ffiStrictHashVerification = False
  , ffiStrictObjectCreation   = False
  }

-- | libgit2's own out-of-the-box defaults (both checks on) -- pass this
-- to 'openRepository'\/'Runix.Git.runGitFFIPerCall' instead of
-- 'defaultFFIOptions' to benchmark against it directly.
libgit2DefaultFFIOptions :: FFIOptions
libgit2DefaultFFIOptions = FFIOptions
  { ffiStrictHashVerification = True
  , ffiStrictObjectCreation   = True
  }

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

-- | Applies 'FFIOptions' -- @git_libgit2_opts@ options are process-global
-- and mutable at any time (not just once at process init), so this can
-- run again with different values later in the same process, e.g. to
-- compare 'defaultFFIOptions' against 'libgit2DefaultFFIOptions' within
-- one benchmark run.
applyFFIOptions :: FFIOptions -> IO ()
applyFFIOptions opts = do
  -- See 'defaultFFIOptions's own doc for why this codebase disables
  -- this by default, and 'gitOptEnableStrictHashVerification's own doc
  -- (@git_odb_read@ pays a redundant SHA1 re-check on every read when
  -- this is on) for the mechanism.
  rcOpts <- c_git_libgit2_opts_set_int gitOptEnableStrictHashVerification (boolToOpt (ffiStrictHashVerification opts))
  unless (rcOpts >= 0) (throwLastError "git_libgit2_opts(GIT_OPT_ENABLE_STRICT_HASH_VERIFICATION)" rcOpts)
  -- Same story for ref writes: 'createOrUpdateRef' -> @git_reference_create@
  -- validates its target object exists (@git_object__is_valid@,
  -- @vendor/libgit2/src/libgit2/refs.c@) before writing the ref --
  -- another real @git_odb_read_header@ call, with the exact same
  -- freshen\/refresh-on-miss cost strict hash verification guards
  -- against on the read side, paid on every single ref write when this
  -- is on.
  rcOpts2 <- c_git_libgit2_opts_set_int gitOptEnableStrictObjectCreation (boolToOpt (ffiStrictObjectCreation opts))
  unless (rcOpts2 >= 0) (throwLastError "git_libgit2_opts(GIT_OPT_ENABLE_STRICT_OBJECT_CREATION)" rcOpts2)
  where
    boolToOpt True  = 1
    boolToOpt False = 0

-- | Open the repository at @path@, creating a bare repository there first
-- if none exists yet -- the FFI-native, subprocess-free equivalent of
-- 'Runix.Git.ensureRepoExists'. Frees the repository handle when the
-- action finishes. 'openRepository'/'closeRepository' are exposed
-- separately for 'Runix.Git.runGitFFIPerCall', which needs a
-- 'Polysemy.Resource.bracket' (a 'Sem'-level bracket, not this plain-'IO'
-- one) around the open handle.
withRepository :: FFIOptions -> FilePath -> (RepoHandle -> IO a) -> IO a
withRepository opts path = bracket (openRepository opts path) closeRepository

closeRepository :: RepoHandle -> IO ()
closeRepository (RepoHandle repo odb) = do
  c_git_odb_free odb
  c_git_repository_free repo

-- | 'applyFFIOptions' runs every call (cheap -- see its own doc for why
-- that's safe), not just the first, so opening a second repository with
-- different 'FFIOptions' later in the same process actually takes
-- effect -- these are process-global libgit2 settings, so the most
-- recent 'openRepository' call's choice always wins for every repository
-- open at once, not just its own.
openRepository :: FFIOptions -> FilePath -> IO RepoHandle
openRepository opts path = do
  libgit2Init
  applyFFIOptions opts
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
-- 'Runix.Git.Hash.hashObject' on the same content. A hand-rolled,
-- @git_odb_write@-bypassing direct loose-object writer (mirroring
-- 'Runix.Git.Store') was tried and measured no better once
-- 'isAncestorOfAny's real dominant cost was fixed (see its own doc) --
-- not worth maintaining two object-writing paths for.
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
-- -- 'Runix.Git.IsAncestorOfAny's full check. ONE @git_revwalk@ pass over
-- @headHex@'s ancestry, checked against every target incrementally
-- (short-circuiting the walk itself on the first hit) -- NOT one
-- 'isDescendantOfOrEqual' (@git_graph_descendant_of@) call per target,
-- which is what this did until a real-repo profile found it responsible
-- for 72% of total wall-clock in a live cascade: each call walks the
-- ancestry graph again from scratch, so N targets meant N full walks
-- instead of one. Mirrors 'Runix.Git.runGitIOWith's own 'IsAncestorOfAny'
-- case, which already gets this for free from a single @git rev-list@
-- call -- the actual, measured root cause of a live-production report
-- that this FFI interpreter still ran far slower than the CLI one on
-- real, ancestry-check-heavy cascades, despite every raw read/write
-- benchmark favouring it.
isAncestorOfAny :: RepoHandle -> [Text] -> Text -> IO Bool
isAncestorOfAny _ [] _ = return False
isAncestorOfAny (RepoHandle repo _) targets headHex =
  allocaBytes oidSize $ \nextOidPtr ->
    alloca $ \walkPtrPtr ->
      bracket
        (do checkCall "git_revwalk_new" =<< c_git_revwalk_new walkPtrPtr repo
            peek walkPtrPtr)
        c_git_revwalk_free
        (\walk ->
          withOid headHex $ \headOidPtr -> do
            checkCall "git_revwalk_push" =<< c_git_revwalk_push walk headOidPtr
            go walk nextOidPtr targetSet)
  where
    targetSet = Set.fromList targets

    go walk oidPtr targetSet' = do
      rc <- c_git_revwalk_next oidPtr walk
      if rc == gitITEROVER
        then return False
        else do
          checkCall "git_revwalk_next" rc
          hex <- oidToBytes <$> BS.packCStringLen (castPtr oidPtr, oidSize)
          if Set.member hex targetSet'
            then return True
            else go walk oidPtr targetSet'

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
