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
  , findTickFrom
  , findTick
  , FileTick(..)
  , fileTicksOf
  , recentAtomsOf
  , encodeTickData
  ) where

import Prelude hiding (drop, readFile, writeFile)

import Control.Monad.State.Strict (lift)
import qualified Data.List as List
import Data.Maybe (fromMaybe, isJust)
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

-- | @fieldLines@ is never empty — 'Storyteller.Core.Types.encodeDraft'
--   always folds a @"type"@ entry into 'ST.tickFields' first — so a blank
--   line separating the header block from the payload is always both
--   present and correctly placed: it's the first one 'decodeTickData'
--   finds scanning forward, full stop, with nothing upstream of it to
--   produce a false match. (An earlier version of this convention only
--   inserted that blank line when *other* fields were also present,
--   collapsing tag and payload onto adjacent lines with no separator at
--   all whenever a tick had none — see 'decodeTickData's own Haddock for
--   what that made possible to get wrong.)
--
--   Fails (via 'MonadFail') if any field's key or value itself contains a
--   newline -- a general invariant of this encoding, not something any one
--   'TickType' instance is responsible for remembering: the header block
--   is one line per field, so a field value with an embedded newline would
--   either get silently truncated (the rest parses as a bogus, colon-less
--   header line and is dropped -- see 'decodeTickData') or, worse, produce
--   a spurious blank line that 'decodeTickData' mistakes for the real
--   header\/payload boundary, swallowing everything genuinely after it
--   (later fields, the real message) into what it thinks is this field's
--   own tail. A 'TickType' with free-form, possibly-multiline text to
--   carry (e.g. 'Storyteller.Writer.Types.CharacterAnswer's question) must
--   put it in the message, the one part of this format that's read
--   verbatim to the end with no further line-based parsing -- never in a
--   field.
encodeTickData :: MonadFail m => ST.TickData -> m Text
encodeTickData td
  | any invalidField (ST.tickFields td) =
      fail ("encodeTickData: a field key or value contains a newline: " <> show (ST.tickFields td))
  | otherwise =
      let fieldLines = map (\(k, v) -> k <> ":" <> v) (ST.tickFields td)
      in return (T.intercalate "\n" fieldLines <> "\n\n" <> ST.tickMessage td)
  where
    invalidField (k, v) = T.any (== '\n') k || T.any (== '\n') v

-- | Split @raw@ at its header\/payload boundary: the first blank line,
--   full stop ('T.breakOn "\n\n"', not 'T.lines'\/'break T.null' — the
--   difference matters for a payload with a trailing newline or blank
--   line of its own, which the latter would silently reshape on
--   rejoining). Every header line is parsed as a @key:value@ field
--   (including @"type"@ — see 'encodeTickData's Haddock for why this
--   boundary is always the right one to use), and the payload — after it,
--   untouched — becomes 'ST.tickMessage' verbatim, no tag left embedded
--   in it to strip back off.
--
--   A message with no blank line at all is malformed input this
--   convention never produces itself (kept as a defensive fallback, not a
--   case any real caller should hit): treated as an untagged, fieldless
--   message rather than failing outright.
decodeTickData :: Text -> ST.TickData
decodeTickData raw =
  let (headerBlock, rest0) = T.breakOn "\n\n" raw
      fields = [ (k, T.drop 1 v)
               | l <- T.lines headerBlock
               , let (k, v) = T.breakOn ":" l
               , not (T.null v) ]
  in if T.null rest0
       then ST.TickData { ST.tickRefs = [], ST.tickFields = [], ST.tickMessage = raw }
       else ST.TickData { ST.tickRefs = [], ST.tickFields = fields, ST.tickMessage = T.drop 2 rest0 }

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
storeAs a = do
  msg <- encodeTickData td
  store (NonAtom (map coerceRef (ST.tickRefs td)) msg)
  where td = toDraft a

-- ---------------------------------------------------------------------------
-- Reading
-- ---------------------------------------------------------------------------

-- | @h@'s own commit, decoded as a typed 'Tick' -- an 'Atom'\'s @"file"@
--   field is reconstructed ('Storage.Core' strips it off into 'atomPath'
--   directly on the way in, along with any other header field into
--   'atomTags' -- e.g. a "hide" tag) alongside a @"type":"atom"@ entry, so
--   it decodes via 'Storyteller.Core.Atom.Atom's own 'TickType' instance
--   the same way any other tick kind does; 'Binary' (a deliberately
--   introduced, recognized kind) gets the same @"type"@ treatment.
--   'Opaque' does not: it's the fallthrough for content we didn't
--   introduce at all (an external edit, legacy data, ...) and make no
--   decoding guarantees about, not a real registered kind -- giving it a
--   @"type"@ entry would falsely claim it's decodable the way an actual
--   'TickType' is, so it gets an empty, content-free 'ST.TickData'
--   instead (see 'Storage.Core.Tick's own Haddock for why neither this nor
--   'Binary' carries prose content this layer would ever show); anything
--   else is decoded via 'decodeTickData'.
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
          , ST.tickFields  = ("type", "atom") : ("file", T.pack path) : tags
          , ST.tickMessage = content
          }
        Binary _ path -> ST.TickData
          { ST.tickRefs    = ST.posRefs pos
          , ST.tickFields  = [("type", "binary"), ("file", T.pack path)]
          , ST.tickMessage = ""
          }
        Opaque _ -> ST.TickData
          { ST.tickRefs    = ST.posRefs pos
          , ST.tickFields  = []
          , ST.tickMessage = ""
          }
        NonAtom _ raw -> (decodeTickData raw) { ST.tickRefs = ST.posRefs pos }
  return ST.Tick { ST.tickPos = pos, ST.tickData = td }
  where
    listToMaybe []      = Nothing
    listToMaybe (x : _) = Just x

-- | Head's own tick, decoded.
getTypesTick :: StoreM m => StoreT m ST.Tick
getTypesTick = headHash >>= readTypesTick

-- | Walk the chain backward from @start@, decoding one tick at a time via
--   'readTypesTick' and stopping at the first one @f@ answers with a
--   'Just' -- the short-circuiting counterpart to 'fileTicksOf': when the
--   answer only ever depends on the most recent matching tick (a "what's
--   this character's last word" or "is there a trailing X" query),
--   this pays for one read per step until found, rather than
--   materializing -- and cross-referencing -- the whole chain up front the
--   way 'fileTicksOf' always must (it answers a different question: not
--   "what's true as of here", but "everything relevant to this path,
--   across the whole history"). Runs out (returns 'Nothing') at root, the
--   same "no earlier state to fall back to" endpoint every hand-rolled
--   walk of this shape already assumed.
findTickFrom :: StoreM m => ObjectHash -> (ObjectHash -> ST.Tick -> Maybe a) -> StoreT m (Maybe a)
findTickFrom start f = go start
  where
    go h = do
      t <- readTypesTick h
      case f h t of
        Just a  -> return (Just a)
        Nothing -> case ST.tickParent t of
          Nothing              -> return Nothing
          Just (TickId parent) -> go (ObjectHash parent)

-- | 'findTickFrom', starting at head.
findTick :: StoreM m => (ObjectHash -> ST.Tick -> Maybe a) -> StoreT m (Maybe a)
findTick f = headHash >>= \h -> findTickFrom h f

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
      allTicks <- mapM (uncurry (toFileTick path)) raw
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

-- | Decode a single commit as a 'FileTick', without regard to whether it's
--   relevant to @path@ at all -- 'ftContent' carries that verdict
--   ('Just' iff this is an atom on exactly @path@), everything else about
--   the shape mirrors 'readTypesTick's per-kind decoding. Shared by
--   'fileTicksOf' (which decodes the whole chain up front) and
--   'recentAtomsOf' (which decodes one commit at a time, lazily, and never
--   further back than it needs to).
toFileTick :: StoreM m => FilePath -> ObjectHash -> CommitData -> StoreT m FileTick
toFileTick path h cd = do
  t <- lift (readTick h)
  let refs = List.drop 1 (commitParents cd)
      (kind, fields, msg, mContent) = case t of
        Atom _ p tags content ->
          ( "atom", ("file", T.pack p) : tags, content
          , if p == path then Just content else Nothing )
        Binary _ p -> ( "binary", [("file", T.pack p)], "", Nothing )
        Opaque _   -> ( "opaque", [], "", Nothing )
        NonAtom _ raw ->
          let td = decodeTickData raw
              -- "type" is exposed via 'ftKind' already -- dropped from
              -- the outward-facing fields so it isn't duplicated on
              -- the wire, same as 'Atom'\/'Binary'\/'Opaque' above
              -- (none of which ever put "type" in their own fields
              -- either).
              otherFields = filter ((/= "type") . fst) (ST.tickFields td)
          in ( fromMaybe "unknown" (lookup "type" (ST.tickFields td))
             , otherFields, ST.tickMessage td, Nothing )
  return FileTick
    { ftTickId  = unObjectHash h
    , ftKind    = kind
    , ftRefs    = map unObjectHash refs
    , ftFields  = fields
    , ftMessage = msg
    , ftContent = mContent
    , ftParent  = case commitParents cd of { [] -> Nothing; (p : _) -> Just (unObjectHash p) }
    }

-- | A curated recent slice of @path@'s own atom history: enough to tell a
--   reader what's changed lately without either its full length or its
--   redundant, already-known-elsewhere bulk. Written for a character's
--   @journal.md@ (see 'Storyteller.Writer.Agent.CharContext'), but nothing
--   here is journal-specific -- it's a general "atoms on this path that
--   carry unique information, in their recent timeline context" query.
--
--   Walks backward from head one commit at a time via 'toFileTick',
--   stopping the moment it has enough rather than materializing the whole
--   chain the way 'fileTicksOf' always must -- the two answer different
--   questions (see 'fileTicksOf's own Haddock), and this one's answer only
--   ever depends on a bounded recent window.
--
--   An atom on @path@ is judged to carry unique information -- and kept --
--   iff it has no cross-reference at all (original content typed directly
--   here, or a reference whose target has since been deleted -- either
--   way, nothing else records this text), or it has one but its content no
--   longer matches any referenced atom's (the user edited a copy, and the
--   /divergence/ from the source is itself the unique part). An atom that
--   still matches a reference verbatim is a plain, unmodified copy --
--   recoverable from its source, so it's dropped on its own, though it may
--   still be pulled in as another atom's padding (below).
--
--   Every kept atom brings up to @padding@ immediate neighbours on each
--   side along -- including ones that don't themselves carry unique
--   information -- purely so the result reads as a coherent span instead
--   of disconnected quotes. Overlapping padding windows from separate kept
--   atoms merge for free: this is a single forward-only walk, not N
--   independent lookups.
--
--   Bounded on two independent axes, whichever is hit first ending the
--   walk: @lookback@ caps how many atoms *on @path@* are ever examined (a
--   hard ceiling on how far into history this looks, however little it's
--   found), and @maxOut@ caps how many atoms the result can ever contain
--   (a hard ceiling on how much lands in a prompt, however much
--   qualifies). In the common case @maxOut@ is hit first, well short of
--   @lookback@ -- which is the point.
--
--   Returned oldest-first, same convention as 'fileTicksOf'.
recentAtomsOf
  :: forall m. StoreM m
  => FilePath  -- ^ file to scan, e.g. "journal.md"
  -> Int       -- ^ lookback: max on-@path@ atoms to examine
  -> Int       -- ^ maxOut: max atoms to return
  -> Int       -- ^ padding: atoms kept on each side of a kept atom
  -> StoreT m [FileTick]
recentAtomsOf path lookback maxOut padding
  | maxOut <= 0 = return []
  | otherwise   = headHash >>= \h -> trimOverflow <$> go h 0 [] [] 0
  where
    trimOverflow acc = let over = length acc - maxOut in if over > 0 then List.drop over acc else acc

    -- @acc@: kept atoms so far, oldest-first (a newly-decided, older
    -- segment is always prepended in front of it -- see the two branches
    -- below). @pending@: the last (at most @padding@) skipped on-@path@
    -- atoms, oldest-first among themselves, held in case the next kept
    -- atom wants them as its newer-side padding. @forceLeft@: atoms still
    -- owed as some earlier kept atom's older-side padding, regardless of
    -- their own status.
    go :: ObjectHash -> Int -> [FileTick] -> [FileTick] -> Int -> StoreT m [FileTick]
    go h examined acc pending forceLeft = do
      cd <- lift (readCommit h)
      ft <- toFileTick path h cd
      let next = case commitParents cd of { [] -> Nothing; (p : _) -> Just p }
      (examined', acc', pending', forceLeft') <-
        if not (isJust (ftContent ft))
          then return (examined, acc, pending, forceLeft)
          else do
            unique <- carriesUniqueInfo ft
            if forceLeft > 0 || unique
              then return
                ( examined + 1
                , (ft : pending) ++ acc
                , []
                , if unique then padding else max 0 (forceLeft - 1)
                )
              else return
                ( examined + 1
                , acc
                , take padding (ft : pending)
                , forceLeft
                )
      if length acc' >= maxOut || examined' >= lookback
        then return acc'
        else maybe (return acc') (\p -> go p examined' acc' pending' forceLeft') next

    carriesUniqueInfo :: FileTick -> StoreT m Bool
    carriesUniqueInfo ft
      | null (ftRefs ft) = return True
      | otherwise = do
          refContents <- mapM (atomContentAt . ObjectHash) (ftRefs ft)
          return (not (any (== Just (ftMessage ft)) refContents))

    atomContentAt :: ObjectHash -> StoreT m (Maybe Text)
    atomContentAt h = do
      t <- lift (readTick h)
      return $ case t of
        Atom _ _ _ content -> Just content
        _                  -> Nothing
