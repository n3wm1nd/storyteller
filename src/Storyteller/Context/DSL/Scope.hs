{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | 'Storyteller.Context.DSL.Compile.treeValueOfCommit', minus every path
--   that's never been atom-tracked at all -- an uploaded binary asset, or
--   anything else that opted out of atom tracking (see
--   'Storyteller.Writer.Agent.ContextFilter.hideBinaryFiles', the
--   equivalent narrowing for a hand-written agent's own
--   'Runix.FileSystem' read).
--
--   This is the *one* piece of context-selection policy that has to be
--   decided in Haskell rather than DSL text: whether a path has ever been
--   atom-tracked is a real storage fact (a chain walk,
--   'Storage.Ops.atomTrackedAmong'), not something a pure filter over a
--   path's own text could ever answer, and it's genuinely never valid DSL
--   content (there's no @Message@ a raw binary blob could sensibly become).
--   Everything else -- which paths count as "lore", "chapters", any other
--   project-specific bucket -- is deliberately *not* mirrored here: that's
--   real content-selection policy, and belongs in ordinary (overridable)
--   DSL text written against the project's own directory conventions
--   (@in chapters/**: ...@), not a second hardcoded classifier standing in
--   for it. A DSL author who can't get the classification they want by
--   glob\/'Storyteller.Context.DSL.Compile.exclude' alone has nothing to
--   reach for here -- by design, not an oversight.
module Storyteller.Context.DSL.Scope
  ( liveTreeValueOfCommit
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops

import Storyteller.Context.DSL.Value

-- | 'Storyteller.Context.DSL.Compile.treeValueOfCommit' with every
--   never-atom-tracked path removed before a DSL program ever gets to see
--   the scope's own key set (glob matching, @for@, all of it) -- a binary
--   file simply never exists as far as any DSL program's own text is
--   concerned, the same absence-not-an-error treatment a missing @read@
--   target already gets.
liveTreeValueOfCommit :: Core.ObjectHash -> Action Value
liveTreeValueOfCommit commit = do
  wt <- liftStore (Core.loadWorkingTree commit)
  let files = [ (path, h) | (path, Core.FSFile h) <- Map.toList wt ]
  tracked <- Action (Ops.atomTrackedAmong (map fst files))
  pure Value
    { valueDefault = pure []
    , valueEntries =
        [ (T.pack path, leafValue . (: []) . FileRead path . TE.decodeUtf8 <$> readBlob h)
        | (path, h) <- files, path `Set.member` tracked
        ]
    }
  where
    readBlob h = liftStore $ Core.readObject h >>= \case
      Core.BlobObject bs -> pure bs
      Core.TreeObject _  -> fail "internal error: file path resolved to a tree object"
