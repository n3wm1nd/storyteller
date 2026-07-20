{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Branch-level business logic specific to the Writer application:
-- tracking files across branches (chapters, entity/tracker branches) and
-- resolving character-generation scenarios. Same shape as
-- 'Server.Core.Branch' — plain 'Sem' functions, no JSON/WebSocket — just too
-- specific to a writing workflow to live in the generic library.
module Server.Writer.Branch
  ( trackFiles
  , charGen
  , importCharacterCard
  , summarize
  , syncTasksOnBranch
  , suggestTasksOnBranch
  , uploadFiles
  , uploadFile
  , uploadImage
  , saveFile
  , saveFileAsNew
  , onlyWhilePresent
  ) where

import Control.Monad (void)
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (showHex)
import Polysemy (Member, Members, Sem)
import Polysemy.Error (throw)
import Polysemy.Fail (Fail)
import Runix.FileSystem (writeFile)
import Runix.Git (Git)
import Runix.Logging (Logging)
import Runix.Random (Random, randomInt)
import System.FilePath (dropExtension, takeFileName, (</>))

import Server.Core.Branch (Main, BranchOpen)
import Server.Core.Run (SessionEffects)
import Server.Core.Util (withBranch)

import qualified Storyteller.Common.Annotation as Annotation
import Storyteller.Writer.Agent.ChapterSummarizer (chapterSummaryGenerate)
import Storyteller.Writer.Agent.LoreSummarizer (loreSummaryGenerate)
import Storyteller.Writer.Agent.JournalSummarizer (journalSummarize, journalChunkAgent, currentSheet)
import Storyteller.Writer.Agent.CharGen (charGenAgent, drawSeed, unSheet, ScenarioTemplate(..), RngSeed(..))
import Storyteller.Writer.Agent.Summarizer (runSummarizer)
import Storyteller.Writer.Agent.Tasks (syncTasks, suggestTasksWith, tasksGenerateAgent)
import Storyteller.Writer.Agent.Tracker (trackBranch)
import Storyteller.Writer.Agent (ContextBlock(..))
import Storyteller.Context.DSL.Value (namedEntry)
import qualified Storyteller.Context.DSL.Render as Render
import qualified Storyteller.Context.DSL.Library as CtxLibrary
import Storyteller.Core.Context (resolveContextQuery, runContextBinding1, runContextValue)
import Storyteller.Writer.Presence (presentAt)
import Storyteller.Writer.Types (Character(..))
import Storyteller.Core.Atom (Atom(..), contentFor)
import Storyteller.Core.Git (BranchOp, BranchTag, runBranchAndFS, runStorage, withStorage)
import Storyteller.Core.Image (Image(..))
import Storyteller.Core.LLM.Role (LLMs)
import Storyteller.Core.Prompt (PromptStorage)
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (BranchName(..), Tick, TickId, fromTick, tickId)
import qualified Data.Yaml as Yaml

import Prelude hiding (writeFile)

data Source
data Tracker
data CharBranch
data ImportBranch

-- | Track a source branch into a single journal file on a target branch.
--   Creates the target branch if it doesn't exist. Returns the destination
--   path tracked into.
--
--   @onlyFile@ restricts the source side to one file (what a manual,
--   user-triggered track wants — limited to whatever's open); 'Nothing'
--   pulls every file on the source branch into the same @toFile@ (what an
--   automatic, write-triggered track wants — a running journal that
--   doesn't lose anything just because the user moved on to another
--   chapter). See 'Storyteller.Writer.Agent.Tracker.trackBranch'.
--
--   Opens its own 'Source'/'Tracker'-tagged scopes rather than an ambient
--   one: the source branch is a different branch entirely, known only from
--   the command payload, and the target may not exist yet (so it can't
--   reuse a scope that assumes the branch is already open and unchanging).
trackFiles
  :: SessionEffects r
  => BranchName        -- ^ target branch
  -> BranchName        -- ^ source branch
  -> Maybe FilePath     -- ^ restrict to one source file; 'Nothing' = every file
  -> FilePath           -- ^ destination file on the target branch
  -> Sem r FilePath
trackFiles target source onlyFile toFile = do
  getBranch target >>= \case
    Nothing -> void $ createBranch target
    Just _  -> return ()
  runBranchAndFS @Source source
    $ runBranchAndFS @Tracker target $ do
        _ <- trackBranch @Source @Tracker onlyFile (onlyWhilePresent (Character target)) toFile
        return toFile

-- | Only copy an atom into @character@'s branch if that character was
--   marked present (see 'Storyteller.Writer.Presence.presentAt') on the
--   atom's own file at the point it was written -- narrative events that
--   happened while the character wasn't in the scene don't belong in their
--   journal. Drops every non-atom tick outright too (presence ticks, notes,
--   ...) -- a character's journal is a copy of narrative content, not a
--   mirror of every tick kind the source branch happens to record. A
--   dropped tick is never marked synced (see
--   'Storyteller.Writer.Agent.Tracker.dropUntilAfterLastSynced'), so a
--   later sync pass simply reconsiders it -- harmless, since presence at a
--   fixed historical position never changes, so it drops again every time.
onlyWhilePresent :: Ops.StoreM m => Character -> Tick -> Ops.StoreT m (Maybe Tick)
onlyWhilePresent character tick = case fromTick @Atom tick of
  Nothing -> pure Nothing
  Just (Atom file _) -> do
    present <- presentAt (tickId tick) file character
    pure (if present then Just tick else Nothing)

-- | Run chargen and commit the result to a branch.
--   Creates the branch if it doesn't exist.
--
--   Opens its own 'CharBranch'-tagged scope for the same reason as
--   'trackFiles': the target branch may need to be created first.
charGen
  :: SessionEffects r
  => BranchName
  -> FilePath
  -> T.Text     -- ^ YAML scenario
  -> Maybe Int  -- ^ RNG seed
  -> Sem r ()
charGen name path scenario seed = do
  template <- case Yaml.decodeEither' (TE.encodeUtf8 scenario) of
    Left  err -> throw (Yaml.prettyPrintParseException err)
    Right val -> return (ScenarioTemplate val)
  getBranch name >>= \case
    Nothing -> void $ createBranch name
    Just _  -> return ()
  runBranchAndFS @CharBranch name $ do
    rngSeed <- maybe drawSeed return (RngSeed <$> seed)
    let sheet = charGenAgent template rngSeed
    writeFile @(BranchTag CharBranch) path (TE.encodeUtf8 (unSheet sheet))
    void $ runStorage @CharBranch (Ops.commitFiles [path])

-- | Create a brand-new character branch and deposit a fixed set of text
--   files, plus an optional binary avatar, onto it in one commit -- the
--   atomic counterpart to a client parsing a SillyTavern-format character
--   card and following up with a plain @CreateBranch@ plus one @saveFile@
--   per generated file and a separate @PUT@ for the avatar, which would
--   leave a half-created character branch visible to every other
--   connection if any step failed partway, and which in practice raced the
--   avatar's own @PUT@ (a second, independent HTTP connection) against
--   this command's branch creation -- see 'Server.Writer.Session.Dispatch'
--   and lib/taverncard.ts's own history. Folding the avatar into this same
--   command removes that race entirely rather than papering over it with
--   retries: a card's avatar is small (comparable to the text fields
--   already carried the same way), so there's no real cost to treating it
--   like the rest of the card instead of like a general-purpose binary
--   upload. The caller is responsible for mapping card fields to file
--   content -- e.g. @sheet.md@ for identity/personality, @instructions.md@
--   for the roleplay-flavored fields a future impersonation agent would
--   consume, an optional lore file for an embedded @character_book@ --
--   this function only knows "branch, files, optional avatar, optional
--   note."
--
--   @note@, when given, lands as a free-floating 'Storyteller.Common.
--   Types.Note' (no refs -- a remark on the import as a whole, not on any
--   one atom) rather than prose in a file: a card's provenance
--   (imported-from/creator attribution) and the creator's own notes are
--   metadata about the import for the human author, not part of what an
--   agent reading @sheet.md@ should treat as the character's identity or
--   voice -- see lib/taverncard.ts's own 'buildImportNote'.
--
--   Unlike 'charGen', does not tolerate the branch already existing --
--   an import always names a fresh branch (checked by the caller before
--   this runs); silently overwriting an existing character's files would
--   be the wrong default for a drag-and-drop import.
importCharacterCard
  :: Members '[Git, StoryStorage, Fail] r
  => BranchName
  -> [(FilePath, T.Text)]
  -> Maybe (FilePath, BS.ByteString)
  -> Maybe T.Text
  -> Sem r ()
importCharacterCard name files avatar note = do
  _ <- createBranch name
  runBranchAndFS @ImportBranch name $ do
    mapM_ (\(path, content) -> writeFile @(BranchTag ImportBranch) path (TE.encodeUtf8 content)) files
    void $ runStorage @ImportBranch (Ops.commitFiles (map fst files))
    case avatar of
      Nothing              -> return ()
      Just (path, content) -> void $ runStorage @ImportBranch (Ops.addBinary path content)
    case note of
      Nothing -> return ()
      Just n  -> runStorage @ImportBranch (Annotation.addNote [] n)

-- | Write one or more files' content directly into the branch, bypassing
--   the chat-agent pipeline entirely (an upload isn't an LLM-authored
--   append, it's raw bytes the user already chose). Runs on the branch
--   scope already open for the caller ('BranchOpen') — the sole caller is
--   'uploadFile' below, which opens that scope itself for the HTTP PUT
--   endpoint's one-shot case.
--
--   Every uploaded path lands as a 'Ops.addBinary' asset — an opaque,
--   path-aware 'Storage.Core.Binary' tick — regardless of whether its
--   content happens to decode as UTF-8. An upload is a deposit, not a
--   claim that the bytes are prose; deciding that is a separate, deliberate
--   "ingest this file" action (not yet built) that promotes a specific
--   path to atom-tracked text on request, not something this function
--   should guess at from the bytes alone.
--
--   Returns the uploaded paths, so the caller can push 'FileAdded' events.
uploadFiles
  :: BranchOpen r
  => [(FilePath, BS.ByteString)] -- ^ (path, content) pairs
  -> Sem r [FilePath]
uploadFiles files = do
  mapM_ (\(path, content) -> runStorage @Main (Ops.addBinary path content)) files
  return (map fst files)

-- | One-shot variant of 'uploadFiles' for the HTTP @PUT /branch/{name}/{path}@
--   endpoint, which has no already-open 'BranchOpen' connection to run
--   against. Opens the branch's scope itself and wraps the write in its own
--   'withStorage' transaction, same as a WS command's per-command 'handle'
--   does (see 'Server.Writer.File.Connection'); that transaction's ref update
--   is what 'Server.Writer.GitWorker'/'notifyRemaps' (wired into every
--   'runAction'/'wsAction' call, including this endpoint's) pick up to notify
--   any connections watching this branch — nothing extra needed here to
--   "poke" them.
uploadFile :: SessionEffects r => T.Text -> FilePath -> BS.ByteString -> Sem r ()
uploadFile branch path content =
  withStorage (withBranch @Main branch (void (uploadFiles [(path, content)])))

-- | Find an unused path under @dir@ for @name@, prefixed with a random
--   8-hex-digit tag to avoid collisions between drops that share a
--   filename (two screenshots both called @image.png@, say). Checked
--   against the branch's current tree rather than assumed unique outright,
--   since a caller could in principle re-drop the exact same tag twice in
--   a row; the loop is only ever expected to run once in practice.
freshAssetPath :: (BranchOpen r, Member Random r) => FilePath -> FilePath -> Sem r FilePath
freshAssetPath dir name = go (8 :: Int)
  where
    go 0 = fail "freshAssetPath: could not find a free asset path"
    go n = do
      tag <- hex8 <$> randomInt
      let candidate = dir </> (tag <> "-" <> name)
      taken <- runStorage @Main (Ops.exists candidate)
      if taken then go (n - 1) else return candidate

    hex8 :: Int -> String
    hex8 i = let h = showHex (i .&. (0xFFFFFFFF :: Int)) ""
              in replicate (8 - length h) '0' <> h

-- | Attach an image to @path@'s timeline: deposits the bytes as their own
--   opaque 'Ops.addBinary' asset under @\<path minus extension\>.assets\/@
--   (same "deposit, not a claim about the bytes" stance as 'uploadFiles'),
--   then records an 'Image' tick on @path@ itself pointing at that asset.
--   Two ticks, two commits (no way to fold a 'Storage.Core.store' call in
--   two) inside one atomic branch-ref transaction, same shape as
--   'importCharacterCard's file-then-avatar sequencing.
attachImage
  :: (BranchOpen r, Member Random r)
  => FilePath -> FilePath -> T.Text -> BS.ByteString -> Sem r ()
attachImage path origName caption content = do
  assetPath <- freshAssetPath (dropExtension path <> ".assets") (takeFileName origName)
  void $ runStorage @Main (Ops.addBinary assetPath content)
  void $ runStorage @Main (Tick.storeAs (Image path assetPath caption))

-- | One-shot variant of 'attachImage' for the HTTP
--   @PUT /branch/{name}/$image/{path}@ endpoint, same one-shot
--   branch-scope-opening shape as 'uploadFile' and for the same reason.
uploadImage :: SessionEffects r => T.Text -> FilePath -> FilePath -> T.Text -> BS.ByteString -> Sem r ()
uploadImage branch path origName caption content =
  withStorage (withBranch @Main branch (attachImage path origName caption content))

-- | One-shot save for the HTTP @PUT /branch/{name}/$raw/{path}@ endpoint —
--   the raw-edit-mode counterpart to 'uploadFile'. Where an upload deposits
--   an opaque 'Ops.addBinary' asset, this reconciles @content@ against the
--   path's existing atom chain via 'Ops.saveFile': a whole-file overwrite
--   from an editor that hands back the entire text still keeps whatever
--   atoms didn't actually change, same as any other reconciliation path
--   (chat regen, upload-then-ingest). Same one-shot branch-scope-opening
--   shape as 'uploadFile', for the same reason (no already-open
--   'BranchOpen' connection for this endpoint to run against).
saveFile :: SessionEffects r => T.Text -> FilePath -> T.Text -> Sem r ()
saveFile branch path content =
  withStorage (withBranch @Main branch (void (runStorage @Main (Ops.saveFile path content))))

-- | One-shot "save as new" for the same @PUT /branch/{name}/$raw/{path}@
--   endpoint, reached via its @?asNew@ query flag instead of a distinct
--   route -- same resource, a different write strategy (see
--   'Storage.Ops.saveFileAsNew'): wholesale replacement, not a reconciled
--   diff against the existing atom chain. @newPath@ defaults to @path@
--   itself (the endpoint's own "asNew" flag carries no destination of its
--   own) -- the common in-place case; a caller wanting to fork to a
--   genuinely different file passes @?newPath=...@ instead. Presence-
--   agnostic, same as 'saveFile' right above it -- 'Ops.saveFileAsNew' is
--   safe to call whether or not @path@ already exists.
saveFileAsNew :: SessionEffects r => T.Text -> FilePath -> FilePath -> T.Text -> Sem r ()
saveFileAsNew branch path newPath content =
  withStorage (withBranch @Main branch (void (runStorage @Main (Ops.saveFileAsNew path newPath content))))

-- | Run one summarization pass for @kind@ against the already-open
--   'Main' branch scope -- see 'Storyteller.Writer.Agent.Summarizer.runSummarizer'.
--   No branch-opening of its own is needed the way 'trackFiles'\/'charGen'
--   need it: everything (source and the resulting 'Storyteller.Common.Summary.Summary'
--   tick) lives on this one already-open branch, and the alternate chain
--   it extends has no branch of its own to open in the first place (see
--   "Storyteller.Common.Summary"'s module Haddock).
--
--   @"prose/chapter"@, @"lore/article"@, and @"journal"@ (see
--   "Storyteller.Writer.Agent.JournalSummarizer"'s own Haddock for why a
--   chunked, unbounded file needs its own bespoke recursive write path
--   rather than a plain 'runSummarizer' @generate@ hook -- it writes
--   directly, at whatever tier(s) actually have new material, rather than
--   going through 'runSummarizer' at all) are the real per-domain
--   summarizers wired in so far. Every other @kind@ still falls back to
--   'passthroughGenerate', a placeholder that copies each touched file's
--   new content across verbatim, grouped by path, with no actual
--   compression -- it exists so an experimental @kind@ is genuinely
--   exercisable end-to-end (a real alternate chain, a real 'Summary' tick,
--   discoverable through 'Storyteller.Writer.Agent.SummaryAccess') before
--   its own per-domain summarizer exists to replace it. Add a new @kind@
--   here the same way once one does; nothing about
--   'Storyteller.Writer.Agent.Summarizer.runSummarizer' itself ever needs
--   to change.
summarize
  :: (LLMs r, Members '[BranchOp Main, Git, StoryStorage, PromptStorage, Logging, Fail] r)
  => T.Text -> Sem r (Maybe TickId)
summarize kind
  | kind == "prose/chapter" = runSummarizer @Main kind (chapterSummaryGenerate @Main kind)
  | kind == "lore/article"  = runSummarizer @Main kind (loreSummaryGenerate @Main kind)
  | kind == "journal"        = do
      sheet <- currentSheet @Main
      Nothing <$ journalSummarize @Main (journalChunkAgent sheet)
  | otherwise                = runSummarizer @Main kind passthroughGenerate

passthroughGenerate :: [Tick] -> Sem r (Map.Map FilePath T.Text)
passthroughGenerate = pure . foldl' step Map.empty
  where
    step acc t = case fromTick @Atom t of
      Just (Atom file _) -> Map.insertWith (flip (<>)) file (contentFor file t) acc
      Nothing             -> acc

-- | Reconcile a tasks.md-shaped file against whatever's new on this
--   already-open 'Main' branch since its last sync -- see
--   'Storyteller.Writer.Agent.Tasks.syncTasks'. Same "restrict to one
--   source file, or every file" shape as 'trackFiles'\/'summarize': a
--   character sidebar's "Sync Tasks" button restricts to its journal, a
--   hypothetical story-branch caller would pass 'Nothing' for "every
--   chapter." Returns whether it made a change, for the dispatch layer to
--   decide whether a 'Server.Writer.Branch.Protocol.FileAdded' is due (the
--   file may not have existed before this call). @fallbackName@ is only
--   ever used when this branch's own @sheet.md@ has no name of its own to
--   give -- see 'Storyteller.Writer.Agent.Tasks.resolveCharacterName'.
syncTasksOnBranch
  :: (LLMs r, Members '[BranchOp Main, PromptStorage, Logging, Fail] r)
  => T.Text -> Maybe FilePath -> FilePath -> Sem r Bool
syncTasksOnBranch fallbackName onlyFile toFile = syncTasks @Main fallbackName (isSourceFile onlyFile toFile) toFile

data LoreSource

-- | Propose new tasks from a full read of this branch's source material,
--   plus -- when @loreSource@ names a (story) branch -- that branch's own
--   world lore, folded in as extra material ahead of it. Deliberately
--   *not* that branch's raw content otherwise: a character's suggestions
--   must only ever be grounded in what they'd actually know -- their own
--   (already presence-gated, see 'Storyteller.Writer.Agent.Tracker's
--   Haddock) journal, plus world knowledge, never scenes they never
--   witnessed. See 'Storyteller.Writer.Agent.Tasks.suggestTasks' and
--   'Server.Writer.Branch.Protocol.SuggestTasks'.
--
--   Opens its own transient 'LoreSource'-tagged scope to read it, same
--   reasoning 'trackFiles' opens 'Source'\/'Tracker' scopes for: the lore
--   branch is a different branch entirely, known only from the command
--   payload. @fallbackName@: see 'syncTasksOnBranch's own Haddock, same
--   reasoning.
suggestTasksOnBranch
  :: (SessionEffects r, Members '[BranchOp Main, PromptStorage, Logging, Fail] r)
  => T.Text -> Maybe BranchName -> FilePath -> Sem r Bool
suggestTasksOnBranch fallbackName loreSource toFile = do
  lore <- maybe (return "") fetchLore loreSource
  let generate cName current material = tasksGenerateAgent cName current (foldLore lore material)
  suggestTasksWith @Main generate fallbackName toFile
  where
    foldLore lore material
      | T.null lore = material
      | otherwise    = lore <> "\n\n---\n\n" <> material

-- | @context.main@'s @"lore"@\/@"other"@ buckets, flattened -- the same
--   @context.main@ definition every other prose path in this application
--   reads through now, not a second independently-hardcoded
--   'Storyteller.Writer.Agent.WorldContext.worldContextOf' read (that
--   module's own notion of "everything eligible that isn't a chapter" had
--   already drifted from @context.main@'s glob-based classification). No
--   real target file to exclude here (@branch@'s own content, not
--   @toFile@, is what's being read), so @path@ is passed as the empty
--   string, which excludes nothing.
fetchLore :: SessionEffects r => BranchName -> Sem r T.Text
fetchLore branch =
  runBranchAndFS @LoreSource branch $ do
    mainBinding <- resolveContextQuery "context.main" (CtxLibrary.toBinding1 CtxLibrary.contextQuery) Nothing
    mainVal     <- runContextBinding1 @LoreSource mainBinding ""
    blocks <- runContextValue @LoreSource $ do
      loreV  <- namedEntry "lore" mainVal
      otherV <- namedEntry "other" mainVal
      concat <$> mapM Render.valueBlocks [loreV, otherV]
    return (T.intercalate "\n\n---\n\n" [ t | ContextBlock t <- blocks ])

-- | 'Just f' restricts to exactly that file; 'Nothing' accepts every file
--   except @toFile@ itself (the tasks file is never its own source) --
--   same "one file, or everything" convention 'trackFiles'\/'trackBranch's
--   @onlyFile@ already uses.
isSourceFile :: Maybe FilePath -> FilePath -> FilePath -> Bool
isSourceFile onlyFile toFile file = case onlyFile of
  Just f  -> file == f
  Nothing -> file /= toFile
