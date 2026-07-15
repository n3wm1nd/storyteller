{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tasks agent: keeps a branch's @tasks.md@ -- short-term goals,
--   long-term goals, and aversions -- roughly in sync with what's actually
--   happened, and (separately) proposes new ones.
--
--   @tasks.md@ is not parsed back into Haskell structure anywhere in this
--   module: it's opaque markdown the model both reads and rewrites in
--   full, the same way 'Storyteller.Writer.Agent.ChapterSummarizer'
--   treats a chapter summary. Per-entry provenance (which bit of the
--   source material an item came from) is likewise the model's own
--   business, written inline as free text -- never a real 'Storage.Core'
--   ref. It's informative, not load-bearing: nothing here parses it back
--   out, and an entry is free to have none at all if there's nothing
--   worth pinning it to.
--
--   The one thing that *is* load-bearing, and *is* a real ref, is the
--   sync marker: same shape as
--   'Storyteller.Writer.Agent.Tracker.trackBranch's own last-synced
--   lookup, but self-referential rather than cross-branch -- after a
--   sync\/suggest pass, a fresh empty 'Storage.Core.Atom' is appended to
--   @tasks.md@ carrying a ref to this branch's own head at the time, so
--   the next pass knows exactly what's new without re-reading anything
--   already accounted for. tasks.md's own visible content is untouched by
--   the marker (empty content), only its ref matters.
--
--   Each pass is a wholesale exchange, never an edit: 'checkpointFile'
--   freezes whatever was there before (fully, with history) behind a
--   fresh boundary, then a plain new atom replaces it -- deliberately the
--   same "recreate, don't patch" choice already made for structured files
--   generally (see @project_checkpoint_saveasnew@), not a special case
--   invented for this module.
module Storyteller.Writer.Agent.Tasks
  ( -- * Effectful glue
    syncTasksWith
  , suggestTasksWith
  , syncTasks
  , suggestTasks
    -- * The two LLM calls
  , tasksReconcileAgent
  , tasksGenerateAgent
    -- * Storage-level pieces (exposed for testing)
  , lastSyncedTasksRef
  , newSourceText
  ) where

import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy
import Polysemy.Fail (Fail)
import Runix.LLM (queryLLM)
import Runix.Logging (Logging, info)
import UniversalLLM (Message(..), ModelConfig(..))

import Storyteller.Core.Atom (Atom(..), contentFor)
import Storyteller.Core.Git (BranchOp, runStorage)
import Storyteller.Core.LLM.Role (LLMs, ProseModel)
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getConfigWithPrompt, getPrompt)
import qualified Storage.Core as Core
import qualified Storage.FS as FS
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Core.Types (Tick, fromTick)

-- ---------------------------------------------------------------------------
-- Storage-level pieces
-- ---------------------------------------------------------------------------

-- | The newest atom on @tasksPath@ that carries a ref, walking back from
--   head -- i.e. this branch's own last sync marker, if it's ever been
--   synced\/suggested before. Filtered to @tasksPath@ specifically (unlike
--   'Storyteller.Writer.Agent.Tracker.trackBranch's tracker-side lookup,
--   which doesn't need to filter because the tracker branch has no other
--   ref-carrying atoms) since a real tasks.md-bearing branch may carry
--   other ref-carrying atoms of its own (e.g. a character branch's journal
--   entries, written by 'Storyteller.Writer.Agent.Tracker').
lastSyncedTasksRef :: Core.StoreM m => FilePath -> Core.StoreT m (Maybe Core.ObjectHash)
lastSyncedTasksRef tasksPath = Core.follow Nothing $ \acc _h t -> case t of
  Core.Atom (r : _) file _ _ | file == tasksPath -> (Just r, False)
  _                                               -> (acc, True)

-- | Every atom-tick among @ticks@ on a file @keep@ accepts (never
--   @tasksPath@ itself, regardless of what @keep@ says -- tasks.md is
--   never its own source material), concatenated in the order given.
--   Pure, so this is the piece pinned directly by unit tests, same
--   "extract pure before wiring" split as
--   'Storyteller.Writer.Agent.ChapterSummarizer.unitSummaryCandidates'.
newSourceText :: (FilePath -> Bool) -> FilePath -> [Tick] -> Text
newSourceText keep tasksPath ticks = T.intercalate "\n\n---\n\n"
  [ contentFor file t
  | t <- ticks
  , Just (Atom file _) <- [fromTick @Atom t]
  , file /= tasksPath
  , keep file
  ]

-- | Read @tasksPath@'s current content, if it exists.
readTasksFile :: Core.StoreM m => FilePath -> Core.StoreT m (Maybe Text)
readTasksFile tasksPath = do
  files <- FS.list
  if tasksPath `elem` files
    then Just . TE.decodeUtf8 <$> Core.readFile tasksPath
    else return Nothing

-- | Replace @tasksPath@ with @newContent@, preserving whatever was there
--   before behind a checkpoint boundary (skipped on a first pass, when
--   there's nothing yet to preserve), then append the sync marker.
exchangeTasksFile :: Core.StoreM m => FilePath -> Bool -> Text -> Core.ObjectHash -> Core.StoreT m ()
exchangeTasksFile tasksPath hadContent newContent markerRef = do
  when hadContent (Ops.checkpointFile tasksPath)
  Ops.saveFileAsNew tasksPath tasksPath newContent
  _ <- Ops.addAtomWithRefs [markerRef] tasksPath ""
  return ()

-- ---------------------------------------------------------------------------
-- Effectful glue
-- ---------------------------------------------------------------------------

-- | Reconcile @tasksPath@ against whatever's new since the last sync,
--   restricted to files @isSource@ accepts (a character branch's journal;
--   'Prelude.const' 'True' for a story branch's every file). No-op (and
--   never touches @tasksPath@) if there's nothing new, or nothing new that
--   @isSource@ accepts -- an unnecessary checkpoint\/recreate cycle would
--   just be noise in the file's history. Returns whether it made a change.
--
--   @reconcile@ is the LLM step, injected rather than called directly so
--   the storage mechanics (marker placement, delta gathering, checkpoint
--   timing) can be pinned by a unit test with a stub in its place --
--   'syncTasks' below is this with the real 'tasksReconcileAgent'.
syncTasksWith
  :: forall branch r
  .  Member (BranchOp branch) r
  => (Text -> Text -> Sem r Text)
  -> (FilePath -> Bool) -> FilePath -> Sem r Bool
syncTasksWith reconcile isSource tasksPath = do
  (mOld, lastSynced, headH) <- runStorage @branch $ do
    old <- readTasksFile tasksPath
    ref <- lastSyncedTasksRef tasksPath
    h   <- Core.headHash
    return (old, ref, h)

  newHashes <- runStorage @branch $
    Core.follow [] $ \acc h _t -> if lastSynced == Just h then (acc, False) else (h : acc, True)

  if null newHashes then return False else do
    newTicks <- runStorage @branch (mapM Tick.readTypesTick newHashes)
    let sourceText = newSourceText isSource tasksPath newTicks
    if T.null (T.strip sourceText) then return False else do
      newContent <- reconcile (fromMaybe "" mOld) sourceText
      runStorage @branch (exchangeTasksFile tasksPath (isJust mOld) newContent headH)
      return True

-- | Propose new tasks: unlike 'syncTasksWith', reads @isSource@'s files in
--   full every time (not just the delta since last sync) -- a deliberate
--   act triggered by the user, not an incremental catch-up, so there's no
--   "since when" to restrict to; see the module Haddock on
--   'Storyteller.Writer.Agent.CharContext.readCharFiles' for the same
--   "explicit call wants everything" reasoning. Still records\/advances
--   the same sync marker 'syncTasksWith' uses, so a reconcile pass run
--   right after doesn't immediately re-process what suggestion just read.
suggestTasksWith
  :: forall branch r
  .  Member (BranchOp branch) r
  => (Text -> Text -> Sem r Text)
  -> (FilePath -> Bool) -> FilePath -> Sem r Bool
suggestTasksWith generate isSource tasksPath = do
  (mOld, sourceText, headH) <- runStorage @branch $ do
    old   <- readTasksFile tasksPath
    files <- FS.list
    texts <- mapM (\p -> TE.decodeUtf8 <$> Core.readFile p)
                  (filter (\p -> p /= tasksPath && isSource p) files)
    h     <- Core.headHash
    return (old, T.intercalate "\n\n---\n\n" texts, h)

  if T.null (T.strip sourceText) then return False else do
    newContent <- generate (fromMaybe "" mOld) sourceText
    runStorage @branch (exchangeTasksFile tasksPath (isJust mOld) newContent headH)
    return True

-- | 'syncTasksWith' with the real reconcile agent.
syncTasks
  :: forall branch r
  .  (LLMs r, Members '[BranchOp branch, PromptStorage, Fail, Logging] r)
  => (FilePath -> Bool) -> FilePath -> Sem r Bool
syncTasks = syncTasksWith @branch tasksReconcileAgent

-- | 'suggestTasksWith' with the real generation agent.
suggestTasks
  :: forall branch r
  .  (LLMs r, Members '[BranchOp branch, PromptStorage, Fail, Logging] r)
  => (FilePath -> Bool) -> FilePath -> Sem r Bool
suggestTasks = suggestTasksWith @branch tasksGenerateAgent

-- ---------------------------------------------------------------------------
-- The two LLM calls
-- ---------------------------------------------------------------------------

-- | Fold new source material into @tasks.md@: mark tasks the new material
--   shows as completed or discarded, adjust ones it shows have changed
--   shape, and leave everything else untouched. Low temperature, same
--   "faithful, repeatable" reasoning as
--   'Storyteller.Writer.Agent.ChapterSummarizer.chapterSummaryAgent' --
--   this is bookkeeping, not creative writing.
tasksReconcileAgent
  :: (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => Text  -- ^ current tasks.md content, or empty if it doesn't exist yet
  -> Text  -- ^ new source material (journal\/story content) since the last sync
  -> Sem r Text
tasksReconcileAgent current newMaterial = do
  configsWithPrompt <- getConfigWithPrompt "agent.tasks.sync" defaultSyncSystemPrompt defaultSyncConfig
  Prompt extraInstructions <- getPrompt "agent.tasks.sync.instructions" defaultSyncInstructions

  info "tasksReconcileAgent: querying model..."
  response <- queryLLM configsWithPrompt [UserText (syncUserMessage current newMaterial extraInstructions)]
  return $ mconcat [ t | AssistantText t <- response ]

-- | Propose new short-term goals, long-term goals, and aversions from a
--   full read of the source material, on top of (not replacing) whatever's
--   already in @tasks.md@. Higher temperature than the reconcile pass --
--   this is meant to actually suggest something, not just transcribe.
tasksGenerateAgent
  :: (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => Text  -- ^ current tasks.md content, or empty if it doesn't exist yet
  -> Text  -- ^ full source material (journal\/story content)
  -> Sem r Text
tasksGenerateAgent current material = do
  configsWithPrompt <- getConfigWithPrompt "agent.tasks.suggest" defaultSuggestSystemPrompt defaultSuggestConfig
  Prompt extraInstructions <- getPrompt "agent.tasks.suggest.instructions" defaultSuggestInstructions

  info "tasksGenerateAgent: querying model..."
  response <- queryLLM configsWithPrompt [UserText (suggestUserMessage current material extraInstructions)]
  return $ mconcat [ t | AssistantText t <- response ]

-- ---------------------------------------------------------------------------
-- Prompts
-- ---------------------------------------------------------------------------

tasksFormatNote :: Text
tasksFormatNote = T.unlines
  [ "Format tasks.md as markdown with exactly three sections, in this order:"
  , "## Short-term goals"
  , "## Long-term goals"
  , "## Aversions"
  , ""
  , "An aversion is an anti-goal: a specific outcome this character (or story)"
  , "expects might happen and is actively steering away from -- not a trait,"
  , "preference, or dislike (\"is squeamish about blood\" is not an aversion;"
  , "\"ending up like her mother, alone and estranged from her own children\" is)."
  , ""
  , "Each section is a bullet list. Each bullet may end with a short parenthetical"
  , "provenance note pointing at what it's based on, e.g. \"(journal, ch. 2)\" --"
  , "include one when there's an obvious source, omit it when there isn't. Never"
  , "invent a citation."
  ]

defaultSyncSystemPrompt :: Prompt
defaultSyncSystemPrompt = Prompt $ T.unlines
  [ "You maintain a tasks.md file tracking a character's (or a story's) goals."
  , tasksFormatNote
  , ""
  , "You are given the current tasks.md and new material written since the last"
  , "update. Rewrite tasks.md to reflect what the new material shows:"
  , "- Remove a task the new material shows was completed."
  , "- Remove a task the new material shows is no longer relevant -- explicitly"
  , "  abandoned, or quietly overtaken by events (a goal the character or story"
  , "  has clearly moved past, even if nothing ever says so directly)."
  , "- Change or adapt a task if the new material shows its shape has shifted --"
  , "  the underlying want is the same but what it would take, or look like, to"
  , "  satisfy it has changed."
  , "- Leave every other task exactly as it is."
  , "- Do not invent new tasks here -- that's a separate pass. Only reconcile"
  , "  what's already listed against what actually happened."
  , "Output only the updated tasks.md content, nothing else."
  ]

defaultSyncConfig :: [ModelConfig ProseModel]
defaultSyncConfig = [MaxTokens 1500, Temperature 0.2]

defaultSyncInstructions :: Prompt
defaultSyncInstructions = ""

syncUserMessage :: Text -> Text -> Text -> Text
syncUserMessage current newMaterial extraInstructions = mconcat
  [ currentSection
  , "New material written since the last update:\n\n" <> newMaterial <> "\n\n"
  , extraInstructionsSection
  , "Write the updated tasks.md."
  ]
  where
    currentSection
      | T.null current = "tasks.md does not exist yet -- there is nothing to reconcile, only to leave empty unless the new material clearly implies a task.\n\n"
      | otherwise       = "Current tasks.md:\n\n" <> current <> "\n\n"
    extraInstructionsSection
      | T.null extraInstructions = ""
      | otherwise                 = extraInstructions <> "\n\n"

defaultSuggestSystemPrompt :: Prompt
defaultSuggestSystemPrompt = Prompt $ T.unlines
  [ "You propose goals for a character (or a story) to strive toward, for a"
  , "tasks.md file."
  , tasksFormatNote
  , ""
  , "You are given the current tasks.md and the full source material (a"
  , "character's journal, or a story's own content) to draw on. That material"
  , "is where this character's personality, current situation, and history of"
  , "behavior actually live -- read it for who they are and how they've been"
  , "acting, not just what's happened to them. Propose new short-term and"
  , "long-term goals, and aversions, that this specific character -- given that"
  , "personality, that situation, that track record -- would actually hold, not"
  , "generic goals any character in a similar plot might have. Ground each one"
  , "in something concrete: an unresolved tension, a stated want, a demonstrated"
  , "fear, a relationship left hanging, a pattern of behavior the material shows"
  , "repeating. Keep every existing task that still holds. Do not repeat a task"
  , "that's already listed in different words. Favor a small number of"
  , "concrete, specific goals over a long list of vague ones -- something a"
  , "story could actually be steered toward, not a mood. Output only the"
  , "updated tasks.md content, nothing else."
  ]

defaultSuggestConfig :: [ModelConfig ProseModel]
defaultSuggestConfig = [MaxTokens 1500, Temperature 0.8]

defaultSuggestInstructions :: Prompt
defaultSuggestInstructions = ""

suggestUserMessage :: Text -> Text -> Text -> Text
suggestUserMessage current material extraInstructions = mconcat
  [ currentSection
  , "Source material:\n\n" <> material <> "\n\n"
  , extraInstructionsSection
  , "Write the updated tasks.md."
  ]
  where
    currentSection
      | T.null current = "tasks.md does not exist yet.\n\n"
      | otherwise       = "Current tasks.md:\n\n" <> current <> "\n\n"
    extraInstructionsSection
      | T.null extraInstructions = ""
      | otherwise                 = extraInstructions <> "\n\n"
