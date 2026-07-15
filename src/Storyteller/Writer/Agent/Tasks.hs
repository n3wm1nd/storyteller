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

-- | The character's real name, read from @sheetPath@'s first @"# "@
--   heading line if present -- the same "first H1 line is the display
--   name" convention already established for @sheet.md@ (see WRITER.md),
--   mirroring the frontend's own @lib/utils.characterDisplayName@. A
--   branch id like @character/mira@ is a slug, not necessarily the
--   character's actual name (a sheet could equally say "Elena Vasquez") --
--   the sheet, when it exists, is the authoritative source. Falls back to
--   @fallbackName@ (the caller's own branch-id-derived guess -- see
--   'Storyteller.Writer.Branches.branchDisplayName') when there's no sheet
--   yet, or the sheet has no heading: a character with no sheet at all is
--   still a real, nameable character worth running Sync\/Suggest for, just
--   with a less specific name to go on until one exists.
firstHeadingName :: Text -> Maybe Text
firstHeadingName content = case filter (T.isPrefixOf "# ") (T.lines content) of
  (h : _) -> Just (T.strip (T.drop 2 h))
  []      -> Nothing

resolveCharacterName :: Core.StoreM m => FilePath -> Text -> Core.StoreT m Text
resolveCharacterName sheetPath fallbackName = do
  files <- FS.list
  if sheetPath `elem` files
    then fromMaybe fallbackName . firstHeadingName . TE.decodeUtf8 <$> Core.readFile sheetPath
    else return fallbackName

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
--   restricted to files @isSource@ accepts (a character branch's journal).
--   No-op (and never touches @tasksPath@) if there's nothing new, or
--   nothing new that @isSource@ accepts -- an unnecessary
--   checkpoint\/recreate cycle would just be noise in the file's history.
--   Returns whether it made a change.
--
--   @reconcile@ is the LLM step, injected rather than called directly so
--   the storage mechanics (marker placement, delta gathering, checkpoint
--   timing) can be pinned by a unit test with a stub in its place --
--   'syncTasks' below is this with the real 'tasksReconcileAgent'.
--   @fallbackName@ is only ever used if @sheet.md@ doesn't exist, or has
--   no heading -- see 'resolveCharacterName'; the character's real name,
--   when available, always wins.
syncTasksWith
  :: forall branch r
  .  Members '[BranchOp branch, Logging] r
  => (Text -> Text -> Text -> Sem r Text)
  -> Text -> (FilePath -> Bool) -> FilePath -> Sem r Bool
syncTasksWith reconcile fallbackName isSource tasksPath = do
  info ("syncTasksWith: checking " <> T.pack tasksPath <> " for new source material...")
  (characterName, mOld, lastSynced, headH) <- runStorage @branch $ do
    name <- resolveCharacterName "sheet.md" fallbackName
    old  <- readTasksFile tasksPath
    ref  <- lastSyncedTasksRef tasksPath
    h    <- Core.headHash
    return (name, old, ref, h)

  newHashes <- runStorage @branch $
    Core.follow [] $ \acc h _t -> if lastSynced == Just h then (acc, False) else (h : acc, True)

  if null newHashes then do
    info "syncTasksWith: nothing new since the last sync, skipping"
    return False
  else do
    newTicks <- runStorage @branch (mapM Tick.readTypesTick newHashes)
    let sourceText = newSourceText isSource tasksPath newTicks
    if T.null (T.strip sourceText) then do
      info ("syncTasksWith: " <> T.pack (show (length newHashes)) <> " new tick(s), none matched by the source filter -- skipping")
      return False
    else do
      newContent <- reconcile characterName (fromMaybe "" mOld) sourceText
      runStorage @branch (exchangeTasksFile tasksPath (isJust mOld) newContent headH)
      info ("syncTasksWith: wrote updated " <> T.pack tasksPath)
      return True

-- | Propose new tasks for this branch's character from a full,
--   *unfiltered* read of every file on the branch (sheet, journal, any
--   other context files) -- deliberately not
--   'Storyteller.Writer.Agent.CharContext.charSummaryWithJournal' (the
--   windowed read 'Server.Writer.File.activeCharacterContext' uses for
--   ambient generation context), even though both read "a character's
--   context": that function's journal windowing also *deduplicates* --
--   'Storage.Tick.recentAtomsOf' drops any journal atom whose content is
--   byte-identical to what it refs, which is every ordinary
--   'Storyteller.Writer.Agent.Tracker'-copied entry (a verbatim copy of
--   its source scene, carrying a ref back to it). That's the right call
--   for ambient generation context, where the source scene is already
--   shown separately (see 'Server.Writer.File.chatWriter's earlier-
--   chapters channel) and re-showing an identical copy would be pure
--   duplication -- but a Suggest pass has no such other channel. Filtered
--   through the same lens, it would see next to nothing: not a smaller
--   version of the journal, effectively no journal at all. The correct
--   primitive for "no other context is coming" is the same one
--   'Storyteller.Writer.Agent.AskCharacter.askCharacterAgent' already
--   uses for exactly that reason -- every file, in full, unfiltered.
--   Still records\/advances the same sync marker 'syncTasksWith' uses, so
--   a reconcile pass run right after doesn't immediately re-process what
--   suggestion just read.
--
--   @fallbackName@: see 'syncTasksWith's own Haddock, same
--   'resolveCharacterName' fallback reasoning.
--
--   No longer takes a generic @isSource@ predicate the way an earlier
--   version did: suggestion is character-specific now -- the generic "or
--   every file, for a story branch" shape was speculative reuse nothing
--   actually exercises yet, so it's gone until something does.
suggestTasksWith
  :: forall branch r
  .  Members '[BranchOp branch, Logging] r
  => (Text -> Text -> Text -> Sem r Text)
  -> Text -> FilePath -> Sem r Bool
suggestTasksWith generate fallbackName tasksPath = do
  info ("suggestTasksWith: reading character context for " <> T.pack tasksPath <> "...")
  (characterName, mOld, sourceText, headH) <- runStorage @branch $ do
    name  <- resolveCharacterName "sheet.md" fallbackName
    old   <- readTasksFile tasksPath
    files <- FS.list
    texts <- mapM (\p -> TE.decodeUtf8 <$> Core.readFile p) (filter (/= tasksPath) files)
    h     <- Core.headHash
    return (name, old, T.intercalate "\n\n---\n\n" texts, h)

  if T.null (T.strip sourceText) then do
    info "suggestTasksWith: no character context found (no sheet, journal, or other files) -- skipping"
    return False
  else do
    newContent <- generate characterName (fromMaybe "" mOld) sourceText
    runStorage @branch (exchangeTasksFile tasksPath (isJust mOld) newContent headH)
    info ("suggestTasksWith: wrote updated " <> T.pack tasksPath)
    return True

-- | 'syncTasksWith' with the real reconcile agent.
syncTasks
  :: forall branch r
  .  (LLMs r, Members '[BranchOp branch, PromptStorage, Fail, Logging] r)
  => Text -> (FilePath -> Bool) -> FilePath -> Sem r Bool
syncTasks = syncTasksWith @branch tasksReconcileAgent

-- | 'suggestTasksWith' with the real generation agent.
suggestTasks
  :: forall branch r
  .  (LLMs r, Members '[BranchOp branch, PromptStorage, Fail, Logging] r)
  => Text -> FilePath -> Sem r Bool
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
--
--   @characterName@ is a plain typed parameter spliced into the fixed
--   message structure below, deliberately *not* a @{{name}}@ hole inside
--   an overridable prompt string -- 'Storyteller.Core.Prompt's own module
--   Haddock rules slotted templates out on purpose ("the structure around
--   that text stays fixed Haskell code, typechecked against whatever
--   values the agent actually has in hand"); an admin-facing override
--   still gets to customize the *wording* via @agent.tasks.sync@\/
--   @agent.tasks.sync.instructions@, same as every other prompt here, just
--   not by templating this identity into it. It matters because @material@
--   (a journal\/story excerpt) is often first-person ("I did...") without
--   ever naming who -- without this, a reconcile pass has no reliable way
--   to know whose tasks it's editing, or that they're this *character's*
--   own in-world motivations at all rather than, say, a writer's own
--   authorial to-do list for the chapter (an easy, real confusion for
--   "tasks" to fall into without being told otherwise).
tasksReconcileAgent
  :: (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => Text  -- ^ the character this tasks.md belongs to
  -> Text  -- ^ current tasks.md content, or empty if it doesn't exist yet
  -> Text  -- ^ new source material (journal content) since the last sync
  -> Sem r Text
tasksReconcileAgent characterName current newMaterial = do
  configsWithPrompt <- getConfigWithPrompt "agent.tasks.sync" defaultSyncSystemPrompt defaultSyncConfig
  Prompt extraInstructions <- getPrompt "agent.tasks.sync.instructions" defaultSyncInstructions

  info "tasksReconcileAgent: querying model..."
  response <- queryLLM configsWithPrompt [UserText (syncUserMessage characterName current newMaterial extraInstructions)]
  return $ mconcat [ t | AssistantText t <- response ]

-- | Propose new short-term goals, long-term goals, and aversions from a
--   full read of the source material, on top of (not replacing) whatever's
--   already in @tasks.md@. Higher temperature than the reconcile pass --
--   this is meant to actually suggest something, not just transcribe.
--   @characterName@: see 'tasksReconcileAgent's own Haddock, same
--   reasoning exactly.
tasksGenerateAgent
  :: (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => Text  -- ^ the character these tasks are for
  -> Text  -- ^ current tasks.md content, or empty if it doesn't exist yet
  -> Text  -- ^ full character context (sheet, other context files, recent journal)
  -> Sem r Text
tasksGenerateAgent characterName current material = do
  configsWithPrompt <- getConfigWithPrompt "agent.tasks.suggest" defaultSuggestSystemPrompt defaultSuggestConfig
  Prompt extraInstructions <- getPrompt "agent.tasks.suggest.instructions" defaultSuggestInstructions

  info "tasksGenerateAgent: querying model..."
  response <- queryLLM configsWithPrompt [UserText (suggestUserMessage characterName current material extraInstructions)]
  return $ mconcat [ t | AssistantText t <- response ]

-- ---------------------------------------------------------------------------
-- Prompts
-- ---------------------------------------------------------------------------

-- | The identity/framing block every tasks.md prompt's *user message*
--   opens with -- who these are for, and the one confusion worth heading
--   off explicitly: "task" is an overloaded word, and without this a model
--   has no reason not to read "tasks.md" as a writer's own authorial
--   to-do list for the chapter (rewrite this scene, tighten that
--   paragraph) rather than what it actually is -- a fictional character's
--   own private, in-world motivations, exactly the kind of thing that
--   belongs in their own head, not a production note.
--
--   Deliberately built into 'syncUserMessage'\/'suggestUserMessage' (the
--   fixed-Haskell-code part assembled fresh every call) rather than folded
--   into 'defaultSyncSystemPrompt'\/'defaultSuggestSystemPrompt' (the
--   *default*, only ever used when 'PromptStorage' has no override
--   committed for @agent.tasks.sync@\/@agent.tasks.suggest@) -- see
--   'Storyteller.Core.Prompt's own module Haddock on why an override is
--   never a slotted template. Baking @characterName@ into the system
--   prompt's default text would make this identity framing silently
--   vanish the moment anyone actually commits an override, which is
--   exactly the failure mode overridable prompts exist to protect
--   against. Living in the user message instead means it's structurally
--   guaranteed to survive any system-prompt override, always.
tasksIdentityNote :: Text -> Text
tasksIdentityNote characterName = T.unlines
  [ "This tasks.md is for a specific fictional character in this story: " <> characterName <> "."
  , "Every task, goal, and aversion in it is " <> characterName <> "'s own private,"
  , "in-world motivation -- something " <> characterName <> " personally wants, is working"
  , "toward, or is trying to avoid, as a person inside the story. This is never a"
  , "writer's own authorial notes, editing to-do list, or production checklist for"
  , "the story itself -- do not propose or reconcile anything about writing,"
  , "revising, or pacing the story. If the source material's first-person \"I\""
  , "voice doesn't otherwise say who's speaking, assume it's " <> characterName <> "."
  ]

tasksFormatNote :: Text
tasksFormatNote = T.unlines
  [ "Format tasks.md as markdown with exactly three sections, in this order:"
  , "## Short-term goals"
  , "## Long-term goals"
  , "## Aversions"
  , ""
  , "An aversion is an anti-goal: a specific outcome this character expects"
  , "might happen and is actively steering away from -- not a trait, preference,"
  , "or dislike (\"is squeamish about blood\" is not an aversion; \"ending up"
  , "like her mother, alone and estranged from her own children\" is)."
  , ""
  , "Each section is a bullet list. Each bullet may end with a short parenthetical"
  , "provenance note pointing at what it's based on, e.g. \"(journal, ch. 2)\" --"
  , "include one when there's an obvious source, omit it when there isn't. Never"
  , "invent a citation."
  , ""
  , "Respond with plain markdown only -- no code fences, no commentary before or"
  , "after it. Your entire response is written verbatim as the new tasks.md file,"
  , "so a wrapping ``` fence would end up saved as part of the file itself."
  ]

defaultSyncSystemPrompt :: Prompt
defaultSyncSystemPrompt = Prompt $ T.unlines
  [ "You maintain a tasks.md file tracking a fictional character's own"
  , "in-world goals."
  , tasksFormatNote
  , ""
  , "You are given the current tasks.md and new material written since the last"
  , "update. Rewrite tasks.md to reflect what the new material shows:"
  , "- Remove a task the new material shows was completed."
  , "- Remove a task the new material shows is no longer relevant -- explicitly"
  , "  abandoned, or quietly overtaken by events (a goal the character has"
  , "  clearly moved past, even if nothing ever says so directly)."
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

syncUserMessage :: Text -> Text -> Text -> Text -> Text
syncUserMessage characterName current newMaterial extraInstructions = mconcat
  [ tasksIdentityNote characterName
  , "\n"
  , currentSection
  , "New material written since the last update:\n\n" <> newMaterial <> "\n\n"
  , extraInstructionsSection
  , "Write " <> characterName <> "'s updated tasks.md."
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
  [ "You propose new goals for a fictional character to strive toward, added to"
  , "their tasks.md."
  , tasksFormatNote
  , ""
  , "You are given the current tasks.md and the character's full context --"
  , "their character sheet, any other notes kept about them, and a recent slice"
  , "of their journal -- to draw on. That material is where this character's"
  , "personality, current situation, and history of behavior actually live --"
  , "read it for who they are and how they've been acting, not just what's"
  , "happened to them. Propose new short-term and long-term goals, and"
  , "aversions, that this specific character -- given that personality, that"
  , "situation, that track record -- would actually hold, not generic goals any"
  , "character in a similar plot might have. Ground each one in something"
  , "concrete: an unresolved tension, a stated want, a demonstrated fear, a"
  , "relationship left hanging, a pattern of behavior the material shows"
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

-- | Ordered material-then-current, deliberately the reverse of
--   'syncUserMessage' above: unlike a reconcile pass's @newMaterial@ (a
--   delta, genuinely unique every call), @material@ here is a character
--   context read (see 'suggestTasksWith') that's byte-identical across
--   repeated Suggest calls whenever nothing new has been appended to the
--   journal in between, and still shares a long common prefix with the
--   last call even when something has (journals are append-only in the
--   common case). @current@ -- last call's own output -- is the one piece
--   guaranteed to differ every time, so it goes last: a provider's
--   prompt-prefix cache can reuse everything through @material@\/
--   @extraInstructions@ and only pay for the short, unique tail. No such
--   ordering helps 'syncUserMessage', whose own @newMaterial@ is already a
--   non-overlapping delta every call -- there's no shared prefix to
--   protect there in the first place. 'tasksIdentityNote' goes first
--   regardless -- it's small and constant per character, not worth
--   reordering around.
suggestUserMessage :: Text -> Text -> Text -> Text -> Text
suggestUserMessage characterName current material extraInstructions = mconcat
  [ tasksIdentityNote characterName
  , "\n"
  , "Source material:\n\n" <> material <> "\n\n"
  , extraInstructionsSection
  , currentSection
  , "Write " <> characterName <> "'s updated tasks.md."
  ]
  where
    currentSection
      | T.null current = "tasks.md does not exist yet.\n\n"
      | otherwise       = "Current tasks.md:\n\n" <> current <> "\n\n"
    extraInstructionsSection
      | T.null extraInstructions = ""
      | otherwise                 = extraInstructions <> "\n\n"
