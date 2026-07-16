{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tasks agent: keeps a branch's @tasks.md@ -- short-term goals,
--   long-term goals, and passive goals -- roughly in sync with what's actually
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
import Data.Maybe (fromMaybe, isJust, listToMaybe)
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

-- | The newest atom on @tasksPath@ that carries a ref -- i.e. this
--   branch's own last sync marker, if it's ever been synced\/suggested
--   before *in the file's current lifetime*. Built on
--   'Storage.Tick.fileTicksOf', which is itself already scoped to the
--   current lifetime (never reads past a removal boundary) -- this
--   function has no idea what a removal marker even is, it only ever asks
--   "what are this file's current ticks" and picks the newest one with a
--   ref. Without that scoping, a marker from a stale, no-longer-visible
--   lifetime (e.g. tasks.md manually cleared via
--   'Storage.Ops.saveFileAsNew' -- see character-sidebar.tsx's
--   TasksEditor, which always bypasses 'exchangeTasksFile') would still be
--   found, wrongly treating a run against the now-empty file as a
--   continuation of a sync history that, as far as the current file is
--   concerned, doesn't exist.
lastSyncedTasksRef :: Core.StoreM m => FilePath -> Core.StoreT m (Maybe Core.ObjectHash)
lastSyncedTasksRef tasksPath = do
  ticks <- Tick.fileTicksOf tasksPath
  return $ listToMaybe
    [ Core.ObjectHash r | ft <- reverse ticks, (r : _) <- [Tick.ftRefs ft] ]

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

  newTicks <- runStorage @branch (Tick.newTypesTicksSince lastSynced)

  if null newTicks then do
    info "syncTasksWith: nothing new since the last sync, skipping"
    return False
  else do
    let sourceText = newSourceText isSource tasksPath newTicks
    if T.null (T.strip sourceText) then do
      info ("syncTasksWith: " <> T.pack (show (length newTicks)) <> " new tick(s), none matched by the source filter -- skipping")
      return False
    else do
      newContent <- reconcile characterName (fromMaybe "" mOld) sourceText
      runStorage @branch (exchangeTasksFile tasksPath (isJust mOld) newContent headH)
      info ("syncTasksWith: wrote updated " <> T.pack tasksPath)
      return True

-- | Propose new tasks for this branch's character. The *first* pass ever
--   (no sync marker yet) reads a full, *unfiltered* dump of every file on
--   the branch -- deliberately not
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
--   duplication -- but a Suggest pass has no such other channel of its
--   own. Filtered through the same lens, it would see next to nothing:
--   not a smaller version of the journal, effectively no journal at all.
--   The correct primitive for a genuinely first read, with nothing to
--   build on yet, is the same one
--   'Storyteller.Writer.Agent.AskCharacter.askCharacterAgent' already uses
--   for exactly that reason -- every file, in full, unfiltered.
--
--   *Every later* pass reuses 'syncTasksWith's own delta machinery
--   instead: only journal\/other-file ticks written since the last
--   sync\/suggest, the same 'Storage.Tick.newTypesTicksSince'\/'newSourceText' this module's
--   own reconcile pass already uses. This is sound, not just cheaper: the
--   character's already-established personality and history aren't lost
--   between calls, they're already folded into @current@ (tasks.md's own
--   prior content, which every earlier pass has already updated) --
--   re-deriving that same understanding from the full journal on every
--   single call was pure waste, and on a long-running character\/a weak
--   local model, the difference between "small delta" and "the whole
--   journal, every time" is the difference between usable and not.
--   @sheet.md@'s own *current* content is still read in full on every
--   pass regardless (small, stable, rarely changes) -- only the
--   unboundedly-growing journal\/other-file material goes through the
--   delta path.
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
  (characterName, mOld, lastSynced, sheetText, headH) <- runStorage @branch $ do
    name  <- resolveCharacterName "sheet.md" fallbackName
    old   <- readTasksFile tasksPath
    ref   <- lastSyncedTasksRef tasksPath
    files <- FS.list
    sheet <- if "sheet.md" `elem` files then TE.decodeUtf8 <$> Core.readFile "sheet.md" else return ""
    h     <- Core.headHash
    return (name, old, ref, sheet, h)

  -- Whether to do a full grounding read or just the delta hinges on
  -- whether this file's *current lifetime* has a sync marker yet --
  -- 'lastSyncedTasksRef' is itself lifetime-scoped (never finds a marker
  -- from before a removal boundary), so a manually-cleared tasks.md (see
  -- character-sidebar.tsx's TasksEditor, whose "Save" always bypasses
  -- 'exchangeTasksFile' via 'Storage.Ops.saveFileAsNew' directly) correctly
  -- reports no marker here, the same as a genuinely first-ever pass would.
  (isFirstPass, bodyText) <- case lastSynced of
    Nothing -> do
      info "suggestTasksWith: first pass for this lifetime of tasks.md -- reading full context..."
      body <- runStorage @branch $ do
        files <- FS.list
        texts <- mapM (\p -> TE.decodeUtf8 <$> Core.readFile p) (filter (\p -> p /= tasksPath && p /= "sheet.md") files)
        return (T.intercalate "\n\n---\n\n" texts)
      return (True, body)
    Just _ -> do
      info "suggestTasksWith: reading what's new since the last sync/suggest..."
      newTicks <- runStorage @branch (Tick.newTypesTicksSince lastSynced)
      return (False, newSourceText (/= "sheet.md") tasksPath newTicks)

  -- A first pass needs *something* at all (a sheet alone is enough to
  -- start from); a later pass needs something genuinely *new* -- sheet.md
  -- alone reappearing unchanged every call isn't a reason to re-run.
  let skip = if isFirstPass
               then T.null (T.strip sheetText) && T.null (T.strip bodyText)
               else T.null (T.strip bodyText)
      sourceText
        | T.null (T.strip sheetText) = bodyText
        | otherwise                   = sheetText <> "\n\n---\n\n" <> bodyText

  if skip then do
    info "suggestTasksWith: no new character context to draw on -- skipping"
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

-- | Propose new short-term goals, long-term goals, and passive goals from a
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
  , "Every task and goal in it is " <> characterName <> "'s own private, in-world"
  , "motivation -- something " <> characterName <> " personally wants, is working"
  , "toward, or would act on if the chance came up, as a person inside the story."
  , "This is never a writer's own authorial notes, editing to-do list, or"
  , "production checklist for the story itself -- do not propose or reconcile"
  , "anything about writing, revising, or pacing the story. If the source"
  , "material's first-person \"I\" voice doesn't otherwise say who's speaking,"
  , "assume it's " <> characterName <> "."
  , ""
  , "tasks.md exists to explain " <> characterName <> "'s observable behavior in the"
  , "current scene -- why they're acting the way they are right now. It is never"
  , "a restatement of who they are in general: a standing personality trait, a"
  , "long-held fear, or \"it's just in their nature\" belongs on their character"
  , "sheet, not here. Only put something in tasks.md if it's a genuinely"
  , "scene-level want or concern that helps explain this scene's behavior on top"
  , "of the sheet -- not a rephrasing of the sheet itself."
  ]

tasksFormatNote :: Text
tasksFormatNote = T.unlines
  [ "Format tasks.md as markdown with exactly three sections, in this order:"
  , "## Short-term goals"
  , "## Long-term goals"
  , "## Passive goals"
  , ""
  , "The first two sections are active: goals this character is presently"
  , "pursuing. The third is passive: something this character isn't currently"
  , "doing anything about, but would act on the moment a specific situation or"
  , "opportunity actually arose."
  , ""
  , "A short-term goal is something this character is actively trying to"
  , "accomplish right now -- within this scene, this beat, or at most by the"
  , "end of the current chapter. It can be small (\"get a decent meal, after"
  , "traveling all day\") as long as it's live right now, not something parked"
  , "for later."
  , ""
  , "It has to be actionable: something this character can actually do, right"
  , "now, in this scene -- not a feeling, a state of mind, or a hope with"
  , "nothing to act on. It also has to be non-obvious: if it's just restating"
  , "what anyone would already assume from what's already happened, it isn't"
  , "worth writing down. A good short-term goal points forward, not backward"
  , "-- it's what the character is now reaching for, not a summary of what"
  , "they already did. It should give the scene somewhere new to go, momentum"
  , "toward something not yet settled, even one they end up failing to"
  , "achieve. It's also fine, often better, for it to be something the"
  , "character isn't announcing -- a private want the other characters"
  , "present in the scene have no particular reason to know about."
  , ""
  , "A long-term goal is not something to accomplish right now, but something"
  , "this character is working toward over time -- concrete and reachable"
  , "within the story itself (\"reach Rivendell\"), never the overall shape of"
  , "the story -- that belongs to the outline, not to any one character's"
  , "private wants."
  , ""
  , "A passive goal is a specific, possible development in this scene that"
  , "this character isn't presently working toward or against and isn't even"
  , "thinking about right now, but would act on immediately if it actually"
  , "arose -- whether that's seizing a chance (\"if left alone with John, would"
  , "finally confront him about the money\") or reacting to something unwanted"
  , "(\"if noticed while sneaking past the guards, would immediately act"
  , "natural\"). It describes a possible situation and the character's"
  , "reaction to it, not a personality trait or standing fear (\"is afraid of"
  , "heights\" belongs on the character sheet, not here -- that's true of them"
  , "everywhere, not a specific thing that could happen in this scene)."
  , ""
  , "It also has to be specific to *this* character, not a reaction anyone"
  , "would have: wanting out of a room that's caught fire isn't a passive"
  , "goal -- that's universal, not a fact about this character in particular."
  , "\"John showing up\" is only worth listing for a character who has a"
  , "reason of their own to react to John in particular; it says nothing true"
  , "unless it's this character's reaction, not anyone's. If a generic person"
  , "dropped into this scene would react the same way, it isn't a passive"
  , "goal."
  , ""
  , "Write it as \"if X happens, does/becomes Y\" -- e.g. \"being noticed while"
  , "sneaking past the guards\" -> \"immediately act natural and stop looking"
  , "for a way through\". The moment the source material shows the triggering"
  , "situation actually arose, that passive goal resolves and is replaced by"
  , "the short-term goal it names -- it doesn't stay listed as a passive goal"
  , "alongside it."
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
  , "- Remove a short- or long-term goal the new material shows was completed."
  , "- Remove a short- or long-term goal the new material shows is no longer"
  , "  relevant -- explicitly abandoned, or quietly overtaken by events (a goal"
  , "  the character has clearly moved past, even if nothing ever says so"
  , "  directly)."
  , "- Change or adapt a goal if the new material shows its shape has shifted --"
  , "  the underlying want is the same but what it would take, or look like, to"
  , "  satisfy it has changed."
  , "- If the new material shows a passive goal's triggering situation"
  , "  actually arose, remove that passive goal and add the short-term goal it"
  , "  names in its place -- it was always meant to convert, not to keep"
  , "  sitting alongside the reaction it predicted."
  , "- Remove a passive goal the new material shows can no longer arise (the"
  , "  situation that could have triggered it is over or gone)."
  , "- Leave every other task or passive goal exactly as it is."
  , "- Do not invent new tasks or passive goals here -- that's a separate pass. Only"
  , "  reconcile what's already listed against what actually happened."
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
  , "of their journal -- to draw on. That material is background, not a"
  , "template to restate: the sheet already covers who this character is --"
  , "their personality, standing fears, general disposition -- and none of that"
  , "belongs in tasks.md on its own. Use it only to judge what a character like"
  , "this would plausibly want or fear happening right now, in their current"
  , "situation. Propose new short-term goals (live in this scene or chapter),"
  , "long-term goals (reachable within the story, never the story's own overall"
  , "arc -- that's the outline's job), and passive goals (dormant unless and"
  , "until a specific scene-level situation actually arises) that this"
  , "specific character -- given that personality, that situation, that track"
  , "record -- would actually hold right now, not generic goals any character"
  , "in a similar plot might have, and not a goal or passive goal the sheet"
  , "already states as a standing trait. Ground each one in something concrete"
  , "and current: an unresolved tension, a stated want, a relationship left"
  , "hanging, a pattern of behavior the material shows repeating. Keep every"
  , "existing task or passive goal that still holds. Do not repeat one that's"
  , "already listed in different words, and do not restate something the"
  , "character sheet already covers. Favor a small number of concrete, specific"
  , "entries over a long"
  , "list of vague ones -- something a story could actually be steered toward,"
  , "not a mood. Output only the updated tasks.md content, nothing else."
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
