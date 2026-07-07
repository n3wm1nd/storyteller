{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bridge between "Storage.Core"'s backend-agnostic 'Tick' (an 'Atom' or
--   an opaque 'NonAtom') and "Storyteller.Core.Types"'s typed-tick
--   vocabulary ('TickType'\/'TickData'\/'Tick') -- the "higher layer"
--   'Storage.Core'\'s own Haddock leaves for someone else to decode a
--   'NonAtom'\'s message. Nothing here is specific to this storage
--   backend beyond that seam: it's the same wire convention
--   "Storyteller.Core.StorageMonad" already used (its own
--   'encodeTickData'\/'decodeTickData'\/'commitToTick'), just retargeted
--   at 'Storage.Core.ObjectHash'\/'MonadStore' instead of
--   'Storyteller.Core.StorageMonad.ObjectHash'\/'MonadGit'.
module Storage.Tick
  ( storeAs
  , getTypesTick
  , readTypesTick
  , FileTick(..)
  , fileTicksOf
  , encodeTickData
  ) where

import Prelude hiding (drop, readFile, writeFile)

import Control.Monad.State.Strict (lift)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Storage.Core
import Storyteller.Core.Types (TickType(..), TickId(..))
import qualified Storyteller.Core.Types as ST

-- ---------------------------------------------------------------------------
-- Wire encoding -- unchanged from "Storyteller.Core.StorageMonad"'s own
-- 'encodeTickData'\/'decodeTickData': a plain text convention, not tied to
-- any storage backend, describing how a typed tick's fields and message
-- share one commit message string.
-- ---------------------------------------------------------------------------

encodeTickData :: ST.TickData -> Text
encodeTickData td =
  let fieldLines = map (\(k, v) -> k <> ":" <> v) (ST.tickFields td)
      body       = ST.tickMessage td
  in if null fieldLines then body else T.intercalate "\n" fieldLines <> "\n\n" <> body

decodeTickData :: Text -> ST.TickData
decodeTickData raw =
  let (headers, remainder) = break T.null (T.lines raw)
      fields = [ (k, T.drop 1 rest)
               | l <- headers
               , let (k, rest) = T.breakOn ":" l
               , not (T.null rest) ]
      msg = case remainder of
        []      -> raw
        (_ : _) -> T.drop (headerByteLen headers) raw
  in ST.TickData { ST.tickRefs = [], ST.tickFields = fields, ST.tickMessage = msg }
  where
    headerByteLen hs = sum (map ((+ 1) . T.length) hs) + 1

tagOf :: Text -> Maybe Text
tagOf msg = case T.lines msg of
  (l : _) | "type:" `T.isPrefixOf` l -> Just (T.drop 5 l)
  _                                    -> Nothing

stripTypeTag :: Text -> Text -> Text
stripTypeTag kind msg = fromMaybe msg (T.stripPrefix ("type:" <> kind <> "\n") msg)

coerceRef :: TickId -> ObjectHash
coerceRef (TickId t) = ObjectHash t

uncoerceRef :: ObjectHash -> TickId
uncoerceRef (ObjectHash t) = TickId t

-- ---------------------------------------------------------------------------
-- Storing
-- ---------------------------------------------------------------------------

-- | Store any 'TickType' value as an opaque 'NonAtom'. For a tick kind
--   with a real file diff of its own (an 'Storyteller.Core.Atom.Atom'),
--   go through "Storage.Ops"'s 'Storage.Ops.addAtom'\/'editAtom' instead
--   -- same as "Storyteller.Core.StorageMonad" itself: its own 'storeAs'
--   has never needed to special-case that kind, since nothing calls it
--   with one.
storeAs :: (StoreM m, TickType a) => a -> StoreT m ObjectHash
storeAs a = store (NonAtom (map coerceRef (ST.tickRefs td)) (encodeTickData td))
  where td = toDraft a

-- ---------------------------------------------------------------------------
-- Reading
-- ---------------------------------------------------------------------------

-- | @h@'s own commit, decoded as a typed 'Tick' -- an 'Atom'\'s "type:atom\n"
--   tag and "file" field are reconstructed ('Storage.Core' strips both off
--   into 'atomPath'\/'atomContent' directly on the way in, and any other
--   header field into 'atomTags' -- e.g. a "hide" tag); anything else is
--   decoded via 'decodeTickData'.
readTypesTick :: StoreM m => ObjectHash -> StoreT m ST.Tick
readTypesTick h = do
  t  <- lift (readTick h)
  cd <- lift (readCommit h)
  let parents = commitParents cd
      pos = ST.TickPos
        { ST.posId     = uncoerceRef h
        , ST.posParent = uncoerceRef <$> listToMaybe parents
        , ST.posRefs   = map uncoerceRef (List.drop 1 parents)
        }
      td = case t of
        Atom _ path tags content -> ST.TickData
          { ST.tickRefs    = ST.posRefs pos
          , ST.tickFields  = ("file", T.pack path) : tags
          , ST.tickMessage = "type:atom\n" <> content
          }
        NonAtom _ raw -> (decodeTickData raw) { ST.tickRefs = ST.posRefs pos }
  return ST.Tick { ST.tickPos = pos, ST.tickData = td }
  where
    listToMaybe []      = Nothing
    listToMaybe (x : _) = Just x

-- | Head's own tick, decoded.
getTypesTick :: StoreM m => StoreT m ST.Tick
getTypesTick = headHash >>= readTypesTick

-- ---------------------------------------------------------------------------
-- File-tick projection -- ported from "Storyteller.Core.StorageMonad"'s
-- walkFileTicks\/toFileTick\/expandRefs\/relinkParents (pure list logic
-- once fed a tick list, unchanged; only the per-commit decoding is
-- retargeted at 'Storage.Core').
-- ---------------------------------------------------------------------------

-- | A single tick entry from the file-tick projection of a branch.
--   Oldest-first when returned by 'fileTicksOf'. Atoms on the queried
--   path have @ftContent = Just blobSuffix@; every other tick has
--   'Nothing', including atoms on a *different* path.
data FileTick = FileTick
  { ftTickId  :: Text
  , ftKind    :: Text
  , ftRefs    :: [Text]
  , ftFields  :: [(Text, Text)]
  , ftMessage :: Text
  , ftContent :: Maybe Text
  , ftParent  :: Maybe Text
  } deriving (Show, Eq)

-- | Walk the branch history from head and extract all ticks relevant to
--   @path@: every atom on that path, everything (transitively) referenced
--   by one, and every other tick whose own "file" field hints at @path@.
--   Returns oldest-first, root included (harmless -- it never carries
--   file content or a "file" field, so it's never a member of the
--   projection, only ever a potential, and here unused, parent link).
fileTicksOf :: StoreM m => FilePath -> StoreT m [FileTick]
fileTicksOf path = do
  h <- headHash
  collectChain h [] >>= go
  where
    collectChain :: StoreM m => ObjectHash -> [(ObjectHash, CommitData)] -> StoreT m [(ObjectHash, CommitData)]
    collectChain h acc = do
      cd <- lift (readCommit h)
      case commitParents cd of
        []      -> return ((h, cd) : acc)
        (p : _) -> collectChain p ((h, cd) : acc)

    go :: StoreM m => [(ObjectHash, CommitData)] -> StoreT m [FileTick]
    go raw = do
      allTicks <- mapM (uncurry toFileTick) raw
      let fileHint   = T.pack path
          atomIds    = [ ftTickId ft | ft <- allTicks, ftContent ft /= Nothing ]
          memberIds  = expandRefs atomIds allTicks
          fileHinted = [ ftTickId ft | ft <- allTicks
                                     , ftContent ft == Nothing
                                     , lookup "file" (ftFields ft) == Just fileHint ]
          included   = Set.fromList (memberIds ++ fileHinted)
      return (relinkParents included Nothing allTicks)

    relinkParents :: Set.Set Text -> Maybe Text -> [FileTick] -> [FileTick]
    relinkParents _ _ [] = []
    relinkParents included lastIncluded (ft : rest)
      | Set.member (ftTickId ft) included =
          ft { ftParent = lastIncluded } : relinkParents included (Just (ftTickId ft)) rest
      | otherwise = relinkParents included lastIncluded rest

    expandRefs :: [Text] -> [FileTick] -> [Text]
    expandRefs members ticks =
      let step ms = ms ++ [ ftTickId ft
                           | ft <- ticks
                           , ftTickId ft `notElem` ms
                           , any (`elem` ms) (ftRefs ft) ]
      in step (step members)

    toFileTick :: StoreM m => ObjectHash -> CommitData -> StoreT m FileTick
    toFileTick h cd = do
      t <- lift (readTick h)
      let refs = List.drop 1 (commitParents cd)
          (fields, msg, mContent) = case t of
            Atom _ p tags content ->
              ( ("file", T.pack p) : tags, "type:atom\n" <> content
              , if p == path then Just content else Nothing )
            NonAtom _ raw ->
              let td = decodeTickData raw in (ST.tickFields td, ST.tickMessage td, Nothing)
          kind = fromMaybe "unknown" (tagOf msg)
      return FileTick
        { ftTickId  = unObjectHash h
        , ftKind    = kind
        , ftRefs    = map unObjectHash refs
        , ftFields  = fields
        , ftMessage = stripTypeTag kind msg
        , ftContent = mContent
        , ftParent  = case commitParents cd of { [] -> Nothing; (p : _) -> Just (unObjectHash p) }
        }
