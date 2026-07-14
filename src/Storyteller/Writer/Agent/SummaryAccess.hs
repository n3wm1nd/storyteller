{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Agent-facing entry points for *using* summaries, once some
--   "Storyteller.Writer.Agent.Summarizer" has produced them -- deliberately
--   the part of this subsystem that needs the most polish, since the whole
--   point of summarization is to let a context-assembly agent say "this is
--   getting long, use the summarized version" or "budget is 5k tokens,
--   compress until it fits" without knowing anything about how many
--   summarizers exist or what they're called.
--
--   Everything here runs inside a caller's already-open @source@ branch
--   scope ('Storyteller.Core.Git.BranchOp') -- there is no branch to open
--   *for* an alternate chain (see "Storyteller.Common.Summary"'s module
--   Haddock: it has no ref of its own), so every tier of summary a caller
--   wants considered lives as a 'Summary' tick on that one open scope,
--   distinguished only by 'summaryKind'. A hierarchy is therefore just an
--   explicit, ordered list of kinds, finest first -- there's no nested
--   branch structure left to discover it from.
--
--   Every function here that returns file content ('contentAt',
--   'densestWithin', 'densest', 'withinBudget') returns *complete* content
--   by construction -- never missing whatever's been written since a
--   summary level was last produced (see 'completeContents'). That's a
--   standing guarantee of this module, not a per-function distinction, so
--   it isn't spelled out in every name.
module Storyteller.Writer.Agent.SummaryAccess
  ( ZoomLevel(..)
  , zoomLevels
  , contentAt
  , densestWithin
  , densest
  , withinBudget
  ) where

import Prelude hiding (readFile)

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Polysemy (Member, Sem)

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Common.Summary
  (Summary(..), lastSummaryOf, lastTouchedIn, previewPath, summaryContent, summaryTickFor, ticksSince)
import Storyteller.Core.Atom (contentFor)
import Storyteller.Core.Git (BranchOp, runStorage)

-- | One rung on the "how compressed can this get" ladder for a given
--   file: either the raw branch itself ('zlSummary' = 'Nothing'), or one
--   summarizer's 'Summary' tick. 'contentAt' reads from whichever this
--   is, uniformly.
data ZoomLevel = ZoomLevel
  { zlSummary :: Maybe Summary  -- ^ 'Nothing' for the raw, unsummarized level
  , zlPreview :: Maybe Text     -- ^ this level's optional blurb -- see 'Storyteller.Common.Summary.previewPath'
  } deriving (Show, Eq)

-- | Every zoom level currently available for @path@, most detailed
--   first: the raw branch, then one entry per @kind@ in @kinds@ (given
--   finest-first) that both has a 'Summary' tick at all and actually
--   covers @path@ in its alternate chain. Stops at the first @kind@ in
--   the list that's missing either -- a coarser tier is expected to have
--   been built *from* a finer one existing, so a gap there means nothing
--   coarser was ever produced. Cheap: no full file content is read here
--   beyond each level's small, optional preview blurb.
zoomLevels
  :: forall source r
  .  Member (BranchOp source) r
  => [Text] -> FilePath -> Sem r [ZoomLevel]
zoomLevels kinds path = do
  rest <- runStorage @source (go kinds)
  return (ZoomLevel Nothing Nothing : rest)
  where
    go [] = return []
    go (kind : more) = do
      mSum <- lastSummaryOf kind
      case mSum of
        Nothing -> return []
        Just (_, s) -> do
          mContent <- summaryContent s path
          case mContent of
            -- This kind's alternate chain doesn't cover @path@ at all (a
            -- summarizer that only ever touches other files) -- nothing
            -- deeper to offer for *this* file, even if the hierarchy
            -- itself continues for others.
            Nothing -> return []
            Just _  -> do
              preview <- summaryContent s previewPath
              restLevels <- go more
              return (ZoomLevel (Just s) preview : restLevels)

-- | @path@'s current content at one zoom level -- 'Nothing' if that
--   level doesn't have (or never had) this file at all.
contentAt :: forall source r. Member (BranchOp source) r => FilePath -> ZoomLevel -> Sem r (Maybe Text)
contentAt path zl = runStorage @source $ case zlSummary zl of
  Nothing -> do
    there <- Ops.exists path
    if there
      then Just . TE.decodeUtf8With TE.lenientDecode <$> Core.readFile path
      else return Nothing
  Just s -> summaryContent s path

-- | @path@'s content at every zoom level, finest first, each one made
--   *complete* on its own -- content plus whatever's been written since
--   *that file's own* compression was last produced (see
--   'unsummarizedTailSince'). The raw level needs no such repair: reading
--   it live is by definition already current. Every other level would
--   otherwise silently omit its own most recent tail -- a summary only
--   ever covers content that existed the moment its summarizer last ran.
--   This is the one place that fold happens, so 'densestWithin' can pick
--   *any* level and still hand back a genuinely complete answer, not just
--   the coarsest one.
completeContents :: forall source r. Member (BranchOp source) r => FilePath -> [ZoomLevel] -> Sem r [Text]
completeContents path = mapM oneLevel
  where
    oneLevel lvl = do
      content <- fromMaybe "" <$> contentAt @source path lvl
      case zlSummary lvl of
        Nothing -> return content  -- the raw level: already current, nothing to append
        Just s  -> (content <>) <$> unsummarizedTailSince @source s path

-- | @path@'s content, at the *densest* (most compressed) available level
--   (among @kinds@, finest first) that still satisfies @ok@ -- a plain
--   acceptability predicate over the candidate text, e.g.
--   @(<= 500) . wordCount@; this module has no opinion on what
--   "acceptable" means. Every candidate 'completeContents' considers is
--   already complete (see its own Haddock), so the choice of level never
--   trades completeness away for density -- only for whichever's
--   *finest* still satisfying @ok@.
--
--   The two extremes this generalizes are just particular predicates:
--
--   * @ok = const True@ -- satisfied immediately, at the raw level --
--     is "give me the file, unsummarized" ('densest' with the
--     compression turned off).
--   * @ok = const False@ -- never satisfied, so this falls through to
--     the coarsest available level -- is exactly 'densest': "as
--     compressed as this file gets, however that came out."
--   * Anything in between (a token/word-count budget) picks the finest
--     level that fits -- see 'withinBudget'.
--
--   If no level satisfies @ok@ at all, falls back to the coarsest
--   available level anyway (still complete, just not under @ok@) and
--   reports 'False', so the caller knows to fall back to truncation or
--   another strategy of its own rather than being told a silent lie about
--   whether the budget held.
densestWithin
  :: forall source r
  .  Member (BranchOp source) r
  => [Text] -> (Text -> Bool) -> FilePath -> Sem r (Text, Bool)
densestWithin kinds ok path = do
  levels     <- zoomLevels @source kinds path
  candidates <- completeContents @source path levels
  return (pick candidates)
  where
    pick cs = case break ok cs of
      (_, found : _) -> (found, True)
      (_, [])        -> (last cs, False)  -- 'cs' is never empty: 'zoomLevels' always includes the raw level

-- | @path@'s densest complete view -- 'densestWithin' with @ok = const
--   False@, so it always falls through to the coarsest available level.
--   No marker separates the appended tail from the summary proper in the
--   returned 'Text' -- plain concatenation, same convention prose atoms
--   already use for appending.
densest :: forall source r. Member (BranchOp source) r => [Text] -> FilePath -> Sem r Text
densest kinds path = fst <$> densestWithin @source kinds (const False) path

-- | @path@'s content, using the least-compressed level that still fits
--   @budget@ under @estimate@ -- a plain cost function, e.g.
--   @T.length \`div\` 4@ or a real tokenizer. 'densestWithin' with
--   @ok = (<= budget) . estimate@.
withinBudget
  :: forall source r
  .  Member (BranchOp source) r
  => [Text] -> (Text -> Int) -> Int -> FilePath -> Sem r (Text, Bool)
withinBudget kinds estimate budget = densestWithin @source kinds (\t -> estimate t <= budget)

-- | Whatever's been written to @path@ since *its own* compression in
--   @s@'s alternate chain was actually produced -- not since @s@'s own
--   pass ran, which can be later than that (the alternate chain is
--   cumulative: a pass that didn't touch @path@ still carries its older
--   compressed content forward unchanged, so @s@ -- typically the *most
--   recent* pass of its kind -- may hold content from several passes ago).
--   Using @s@'s own tick as the stop point would get the boundary wrong:
--   silently dropping whatever was written to @path@ between the pass
--   that actually produced its current compression and @s@'s own, later,
--   unrelated pass.
--
--   'Storyteller.Common.Summary.lastTouchedIn' finds the exact
--   alternate-chain commit that last changed @path@; 'summaryTickFor'
--   reverses that into the exact source-chain tick it came from. Only
--   *that* tick is a correct stop point for 'ticksSince'. Reconstructed
--   the same way 'Storyteller.Writer.Agent.Tracker.copyAtom' recovers one
--   atom's own content -- concatenating each matching atom's text, oldest
--   first -- since ticks carry their own content verbatim.
unsummarizedTailSince :: forall source r. Member (BranchOp source) r => Summary -> FilePath -> Sem r Text
unsummarizedTailSince s path = runStorage @source $ do
  mTouch   <- lastTouchedIn s path
  mStopTick <- case mTouch of
    Nothing -> return Nothing
    Just h  -> fmap fst <$> summaryTickFor h
  ticks <- ticksSince mStopTick
  return (T.concat (map (contentFor path) ticks))
