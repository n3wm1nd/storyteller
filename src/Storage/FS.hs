-- | Ambient file operations on the working tree that need no access to
--   the object store beyond "Storage.Core"'s own 'readFile'\/'writeFile' --
--   the directory\/listing\/query operations ('createDirectory'\/'remove'\/
--   'removeRecursive'\/'list'\/'isDirectory'\/'listChildren') that read or
--   mutate the ambient tree by path, built entirely on the
--   'getAmbientTree'\/'modifyAmbientTree' seam "Storage.Core" exports for
--   this module.
--
--   Re-exports the related core operations ('readFile'\/'writeFile'\/
--   'reset'\/'inWorktree' and the 'WorkingTree' types) so a caller needs
--   only @"import Storage.FS"@ for the full ambient-file interface. The
--   chain operations themselves ('store'\/'drop'\/'at'\/...) stay in
--   "Storage.Core" -- import that alongside for anything that moves the
--   chain.
module Storage.FS
  ( -- * Ambient file operations
    createDirectory
  , remove
  , removeRecursive
  , list
  , exists
  , isDirectory
  , listChildren

    -- * Re-exported from "Storage.Core": essential file I/O and the
    -- ambient-tree lifecycle these operations live within
  , readFile
  , writeFile
  , reset
  , inWorktree

    -- * Working tree
  , FSNode(..)
  , WorkingTree
  , emptyWorkingTree
  , StoreT
  , StoreM
  , ObjectHash
  ) where

import Prelude hiding (readFile, writeFile)

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import System.FilePath (splitDirectories)

import Storage.Core
  ( StoreT, StoreM, ObjectHash
  , FSNode(..), WorkingTree, emptyWorkingTree
  , getAmbientTree, modifyAmbientTree
  , readFile, writeFile, reset, inWorktree
  )

-- | Introduce @path@ as an explicit, possibly-empty directory entry in
--   the ambient tree.
createDirectory :: Monad m => FilePath -> StoreT m ()
createDirectory path = modifyAmbientTree (Map.insertWith keepExisting path FSDir)

-- | Remove @path@ from the ambient tree.
remove :: Monad m => FilePath -> StoreT m ()
remove path = modifyAmbientTree (Map.delete path)

-- | Remove @path@ and everything under it (files and subdirectory
--   entries alike) from the ambient tree.
removeRecursive :: Monad m => FilePath -> StoreT m ()
removeRecursive path = modifyAmbientTree (Map.filterWithKey keep)
  where
    keep k _ = k /= path && not (isUnderDir path k)
    isUnderDir dir p =
      let dirParts  = splitDirectories dir
          pathParts = splitDirectories p
      in List.take (length dirParts) pathParts == dirParts && length pathParts > length dirParts

-- | Every file path currently in the ambient tree (directories excluded).
list :: Monad m => StoreT m [FilePath]
list = do
  wt <- getAmbientTree
  return [ p | (p, FSFile _) <- Map.toList wt ]

-- | Whether @path@ currently exists as a *file* in the ambient tree --
--   one map lookup, never a scan of 'list'. A directory entry answers
--   'False'; that's 'isDirectory's question.
exists :: Monad m => FilePath -> StoreT m Bool
exists path = do
  wt <- getAmbientTree
  return $ case Map.lookup path wt of
    Just (FSFile _) -> True
    _               -> False

-- | Whether @path@ is an explicit directory entry in the ambient tree.
isDirectory :: Monad m => FilePath -> StoreT m Bool
isDirectory path = do
  wt <- getAmbientTree
  return $ case Map.lookup path wt of
    Just FSDir -> True
    _          -> False

-- | Every file *and* directory entry immediately under @dir@ (its direct
--   children only -- unlike 'list', which is every file anywhere,
--   recursively, with no directories at all). @"/"@\/@"."@\/@""@ all mean
--   the ambient tree's own root.
listChildren :: Monad m => FilePath -> StoreT m [FilePath]
listChildren dir = do
  wt <- getAmbientTree
  return [ p | p <- Map.keys wt, isDirectChild dir p ]
  where
    isDirectChild d p
      | d `elem` ["/", ".", ""] = length (splitDirectories p) == 1
      | otherwise =
          let dirParts  = splitDirectories d
              pathParts = splitDirectories p
          in List.take (length dirParts) pathParts == dirParts
             && length pathParts == length dirParts + 1

keepExisting :: FSNode -> FSNode -> FSNode
keepExisting _ old = old
