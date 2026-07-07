{-# LANGUAGE OverloadedStrings #-}

-- | User-facing operations built entirely from "Storage.Core"'s
-- primitives (store\/drop\/at\/readAt\/reset\/inWorktree\/readFile\/
-- writeFile\/createDirectory\/remove\/list) -- nothing here reaches
-- around them, or touches the chain\/ambient tree any other way.
module Storage.Ops
  ( addAtom
  , findAtom
  , editAtom
  , replaceAtom
  ) where

import Prelude hiding (drop, readFile, writeFile, appendFile)

import Control.Monad.State.Strict (lift)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import Storage.Core

-- | Whether @path@ currently has any content in the ambient tree.
exists :: StoreM m => FilePath -> StoreT m Bool
exists path = elem path <$> list

-- | Append @content@ to @path@ in the ambient tree, creating it if
--   absent: read whatever's there already (if anything), write the
--   extension.
appendFile :: StoreM m => FilePath -> Text -> StoreT m ()
appendFile path content = do
  already <- exists path
  old     <- if already then readFile path else return BS.empty
  writeFile path (old <> TE.encodeUtf8 content)

-- | Commit @content@ as a new atom at @path@, and append the same
--   content to the ambient tree -- two independent steps. The commit is
--   a real chain change ('store'); the append is a plain ambient-tree
--   write. If the ambient tree was already in sync with head, it's still
--   in sync afterward (the same bytes landed in both places); if it
--   wasn't, this doesn't touch whatever else was pending there.
addAtom :: StoreM m => FilePath -> Text -> StoreT m ObjectHash
addAtom path content = do
  newHead <- store (Atom [] path content)
  appendFile path content
  return newHead

-- | The nearest atom at or before @start@, walking backward through
--   parents. Fails once the chain runs out (root reached, no parent
--   left) without finding one.
findAtom :: StoreM m => ObjectHash -> StoreT m ObjectHash
findAtom start = do
  t <- lift (readTick start)
  case t of
    Atom {}    -> return start
    NonAtom {} -> do
      cd <- lift (readCommit start)
      case commitParents cd of
        []      -> fail "findAtom: no atom in history"
        (p : _) -> findAtom p

-- | Apply @f@ to the nearest atom's own content -- walking back from
--   head, skipping over anything since (notes and other bookkeeping
--   ticks aren't the concern here, only the last real content addition
--   is) -- keeping its cross-branch refs and chain position. Returns the
--   new head.
editAtom :: StoreM m => (Text -> Text) -> StoreT m ObjectHash
editAtom f = do
  h      <- headHash
  target <- findAtom h
  at target $ do
    old <- drop
    case old of
      Atom refs path content -> store (Atom refs path (f content))
      NonAtom {}             -> fail "editAtom: findAtom returned a non-atom (unreachable)"

-- | Replace the nearest atom's content outright -- 'editAtom' with a
--   constant function.
replaceAtom :: StoreM m => Text -> StoreT m ObjectHash
replaceAtom = editAtom . const
