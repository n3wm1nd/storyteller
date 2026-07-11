{-# LANGUAGE ForeignFunctionInterface #-}

-- | Raw @foreign import ccall@ declarations for the libgit2 C API. No
-- marshalling logic lives here -- see 'Runix.Git.FFI' for that.
-- Deliberately a small slice of libgit2: exactly what 'Runix.Git''s effect
-- vocabulary needs -- repository bootstrap, object read/write of any
-- kind, ref resolution/creation/deletion/iteration, and ancestry.
-- Opaque handle types live in 'Runix.Git.FFI.Types'.
module Runix.Git.FFI.Raw
  ( c_git_libgit2_init
  , c_git_repository_open
  , c_git_repository_init
  , c_git_repository_free
  , c_git_repository_odb
  , c_git_reference_name_to_id
  , c_git_reference_create
  , c_git_reference_lookup
  , c_git_reference_delete
  , c_git_reference_free
  , c_git_reference_target
  , c_git_reference_name
  , c_git_reference_iterator_glob_new
  , c_git_reference_next
  , c_git_reference_iterator_free
  , c_git_odb_read
  , c_git_odb_write
  , c_git_odb_object_free
  , c_git_odb_object_data
  , c_git_odb_object_size
  , c_git_odb_object_type
  , c_git_odb_free
  , c_git_graph_descendant_of
  , c_git_error_last
  ) where

import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..), CUInt(..))
import Foreign.Ptr (Ptr)

import Runix.Git.FFI.Types
  ( Repository, Odb, OdbObject, Reference, RefIterator, GitOid, GitError )

foreign import ccall safe "git_libgit2_init"
  c_git_libgit2_init :: IO CInt

foreign import ccall safe "git_repository_open"
  c_git_repository_open :: Ptr (Ptr Repository) -> CString -> IO CInt

foreign import ccall safe "git_repository_init"
  c_git_repository_init :: Ptr (Ptr Repository) -> CString -> CUInt -> IO CInt

foreign import ccall safe "git_repository_free"
  c_git_repository_free :: Ptr Repository -> IO ()

foreign import ccall safe "git_repository_odb"
  c_git_repository_odb :: Ptr (Ptr Odb) -> Ptr Repository -> IO CInt

foreign import ccall safe "git_reference_name_to_id"
  c_git_reference_name_to_id :: Ptr GitOid -> Ptr Repository -> CString -> IO CInt

-- | Direct (non-symbolic) ref create-or-force-update; the final
-- @const char*@ is an optional reflog message (passed as 'nullPtr').
foreign import ccall safe "git_reference_create"
  c_git_reference_create
    :: Ptr (Ptr Reference) -> Ptr Repository -> CString -> Ptr GitOid -> CInt -> CString -> IO CInt

foreign import ccall safe "git_reference_lookup"
  c_git_reference_lookup :: Ptr (Ptr Reference) -> Ptr Repository -> CString -> IO CInt

-- | Removes on disk; the handle itself must still be freed afterward --
-- see 'c_git_reference_free'.
foreign import ccall safe "git_reference_delete"
  c_git_reference_delete :: Ptr Reference -> IO CInt

foreign import ccall safe "git_reference_free"
  c_git_reference_free :: Ptr Reference -> IO ()

-- | Returns 'Foreign.Ptr.nullPtr' for a symbolic reference.
foreign import ccall safe "git_reference_target"
  c_git_reference_target :: Ptr Reference -> IO (Ptr GitOid)

foreign import ccall safe "git_reference_name"
  c_git_reference_name :: Ptr Reference -> IO CString

foreign import ccall safe "git_reference_iterator_glob_new"
  c_git_reference_iterator_glob_new :: Ptr (Ptr RefIterator) -> Ptr Repository -> CString -> IO CInt

-- | Returns @GIT_ITEROVER@ (not an error) once exhausted.
foreign import ccall safe "git_reference_next"
  c_git_reference_next :: Ptr (Ptr Reference) -> Ptr RefIterator -> IO CInt

foreign import ccall safe "git_reference_iterator_free"
  c_git_reference_iterator_free :: Ptr RefIterator -> IO ()

foreign import ccall safe "git_odb_read"
  c_git_odb_read :: Ptr (Ptr OdbObject) -> Ptr Odb -> Ptr GitOid -> IO CInt

-- | @git_object_t@'s ABI representation is a plain @int@.
foreign import ccall safe "git_odb_write"
  c_git_odb_write :: Ptr GitOid -> Ptr Odb -> Ptr () -> CSize -> CInt -> IO CInt

foreign import ccall safe "git_odb_object_free"
  c_git_odb_object_free :: Ptr OdbObject -> IO ()

foreign import ccall safe "git_odb_object_data"
  c_git_odb_object_data :: Ptr OdbObject -> IO (Ptr ())

foreign import ccall safe "git_odb_object_size"
  c_git_odb_object_size :: Ptr OdbObject -> IO CSize

-- | @git_object_t@'s ABI representation is a plain @int@ (see 'c_git_odb_write').
foreign import ccall safe "git_odb_object_type"
  c_git_odb_object_type :: Ptr OdbObject -> IO CInt

foreign import ccall safe "git_odb_free"
  c_git_odb_free :: Ptr Odb -> IO ()

-- | 1 if @commit@ descends from @ancestor@, 0 if not (a commit is never
-- its own descendant), or a negative libgit2 error code.
foreign import ccall safe "git_graph_descendant_of"
  c_git_graph_descendant_of :: Ptr Repository -> Ptr GitOid -> Ptr GitOid -> IO CInt

foreign import ccall safe "git_error_last"
  c_git_error_last :: IO (Ptr GitError)
