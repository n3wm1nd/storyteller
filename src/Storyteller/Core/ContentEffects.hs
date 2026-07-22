{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The Polysemy effect vocabulary the context-assembly DSL and its
--   agents are actually written against, replacing direct
--   'Storage.Core.StoreT'\/'Storage.Core.MonadStore' access (see
--   @project_mcp_export_effect_boundary@).
--
--   Each effect here names one genuine, independently-supportable
--   /concept/ a DSL library function or agent asks for -- never a raw
--   storage primitive passed through with a constructor wrapped around
--   it, and never grouped by which module happened to need it first (see
--   the module-level design note this replaced: bundling
--   'Storage.Query.atomTrackedAmong'-style history access with plain file
--   reads into one fat effect just relocates the "can't support half of
--   it" problem one level up). A backend that can genuinely support
--   /some/ of these concepts and not others should be able to provide
--   interpreters for exactly that subset -- see each effect's own
--   Haddock for why it isn't merged with its neighbours.
--
--   Every effect but 'BranchResolve' carries a @(branch :: k)@ phantom,
--   exactly matching 'Storyteller.Core.Branch.BranchOp'\/@Runix.FileSystem@'s
--   own convention: a DSL evaluation isn't pinned to one fixed branch --
--   'Storyteller.Core.Context.runContextValue' is called against
--   @\@Main@, @\@LoreSource@, or a caller-generic @\@branch@ depending on
--   the call site (confirmed by grep across "Server.Writer.*" and
--   "Storyteller.Writer.Agent.AskCharacter") -- so these effects need to
--   coexist, distinctly addressable, the same way two simultaneously-live
--   'BranchOp' scopes already do, rather than being wired to one branch
--   once, globally.
--
--   Every interpreter here is built on 'Storyteller.Core.Branch.runStorage'
--   -- the existing, proven "one 'BranchOp' effect call dispatches a whole
--   closed 'Core.StoreT' computation" mechanism -- so none of this changes
--   how many storage-level operations any of these do, only how a caller
--   names what it wants. The 'Core.StoreT' bodies below are moved
--   verbatim from wherever they used to live as directly-called
--   functions (@treeValueOfCommit@, @charactersInBinding@, @journalDelta@,
--   @readConversation@, @lastSyncedTasksRef@, and 'Storage.Ops'\'s own
--   write primitives), not rewritten.
--
--   Deliberately no dependency on "Storyteller.Core.Git"\/@Runix.Git@
--   anywhere in this module: every interpreter below needs only
--   'Storyteller.Core.Branch.BranchOp' (all of them but 'BranchResolve'),
--   or 'Storyteller.Core.Storage.StoryStorage' ('BranchResolve' alone,
--   which needs branch-name resolution -- not expressible via
--   'Core.StoreM' alone, see that effect's own Haddock). Both are already
--   backend-agnostic effects in their own right; git is merely the one
--   interpreter this codebase happens to supply for them today
--   ("Storyteller.Core.Git"'s @runBranchOpGit@\/@runStoryStorageGit@). Any
--   other 'BranchOp'\/'StoryStorage' interpreter composes under every
--   function here unchanged.
module Storyteller.Core.ContentEffects
  ( -- * Tree snapshots (portable to any content-addressed or plain
    -- directory backend -- no history involved)
    TreeAccess(..)
  , currentHead
  , treeSnapshot
  , readTreeBlob
  , runTreeAccess

    -- * Character presence (tick-history dependent)
  , Presence(..)
  , charactersPresent
  , runPresence

    -- * Curated journal windows (tick-history dependent)
  , JournalAccess(..)
  , journalWindow
  , runJournalAccess

    -- * Turn-shaped conversation history (the one most likely to have a
    -- native, independently-implementable turn representation on a different backend)
  , ConversationAccess(..)
  , Turn(..)
  , conversationTurns
  , runConversationAccess

    -- * Tracked-file policy (tick-history dependent)
  , TrackedFiles(..)
  , atomTrackedAmong
  , runTrackedFiles

    -- * tasks.md's own sync marker (tick-history dependent)
  , TasksSyncTracking(..)
  , lastSyncedTasksRef
  , runTasksSyncTracking

    -- * Committing content (shared write vocabulary)
  , AtomWrite(..)
  , checkpointFile
  , saveFileAsNew
  , addAtom
  , addAtomWithRefs
  , runAtomWrite

    -- * Branch name resolution
  , BranchResolve(..)
  , resolveBranch
  , runBranchResolve

    -- * The whole vocabulary, one branch, one backend
  , runContentEffectsGit
  ) where

import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Polysemy

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import qualified Storage.Query as Query
import qualified Storage.Tick as Tick

import Storyteller.Core.Branch (BranchOp, runStorage)
import Storyteller.Core.Storage (StoryStorage, getBranch)
import Storyteller.Core.Types (Branch(..), BranchName, TickId(..))
import qualified Storyteller.Writer.Presence as WriterPresence
import Storyteller.Writer.Types (Character)

-- ---------------------------------------------------------------------------
-- Tree snapshots
-- ---------------------------------------------------------------------------

-- | A commit's tree, as a flat list of tracked paths (never-atom-tracked
--   paths -- an uploaded binary asset, say -- already excluded, same
--   policy 'Storage.Query.loadLiveWorkingTree' always applied), plus the
--   ability to read one blob and to ask where "here" currently is. No
--   parent-chain walk anywhere in this effect -- a plain directory (or
--   any other content-addressed snapshot) can back all three operations
--   honestly.
data TreeAccess (branch :: k) (m :: Type -> Type) a where
  CurrentHead  :: TreeAccess branch m Core.ObjectHash
  TreeSnapshot :: Core.ObjectHash -> TreeAccess branch m [(FilePath, Core.ObjectHash)]
  ReadTreeBlob :: Core.ObjectHash -> TreeAccess branch m ByteString

makeSem ''TreeAccess

runTreeAccess :: forall branch r a. Member (BranchOp branch) r => Sem (TreeAccess branch ': r) a -> Sem r a
runTreeAccess = interpret $ \case
  CurrentHead      -> runStorage @branch Core.headHash
  TreeSnapshot h   -> runStorage @branch (Query.loadLiveWorkingTree h)
  ReadTreeBlob h   -> runStorage @branch $ lift $ Core.readObject h >>= \case
    Core.BlobObject bs -> pure bs
    Core.TreeObject _  -> fail "readTreeBlob: hash is a tree, not a blob"

-- ---------------------------------------------------------------------------
-- Presence
-- ---------------------------------------------------------------------------

-- | Which characters are tracked\/present in @path@'s own tick history --
--   already-derived, deduplicated character identities
--   ('Storyteller.Writer.Presence.activeCharacters' folded into the
--   interpreter, not left for a caller to redo the enter\/leave-event
--   fold), leaving only the final cosmetic step (branch name -> display
--   name, a Writer-layer presentation concern, not a "who's here"
--   concept) to the caller. The concept a DSL/agent caller actually wants
--   is "who's here," never "here are some ticks, go figure it out
--   yourself." Inherently tick-history dependent -- a backend with no
--   real history has nothing to answer this from.
data Presence (branch :: k) (m :: Type -> Type) a where
  CharactersPresent :: FilePath -> Presence branch m (Set Character)

makeSem ''Presence

runPresence :: forall branch r a. Member (BranchOp branch) r => Sem (Presence branch ': r) a -> Sem r a
runPresence = interpret $ \case
  CharactersPresent path -> WriterPresence.activeCharacters <$> runStorage @branch (Tick.fileTicksOf path)

-- ---------------------------------------------------------------------------
-- Journal windows
-- ---------------------------------------------------------------------------

-- | A curated recent slice of @path@'s own history (see
--   'Storage.Tick.recentAtomsOf' for the lookback\/maxOut\/padding
--   curation itself), returned as the kept entries' own message text,
--   oldest-first -- callers wrap this into whatever presentation shape
--   they need (a DSL 'Storyteller.Context.DSL.Value.Message', a
--   'Storyteller.Writer.Agent.CharContextBlock', ...) with their own
--   framing header, rather than this effect committing to one. Deliberately
--   not the raw 'Storage.Tick.FileTick' list: that's storage-internal
--   vocabulary (@ftKind@, @ftFields@'s hide flag, ...), not something a
--   caller should have to know about to ask "what's new in the journal."
--
--   The @Maybe Core.ObjectHash@ is "read at this commit instead of the
--   caller's own ambient position" -- @Nothing@ for the common case (a
--   caller already scoped to the branch it wants, e.g.
--   'Storyteller.Writer.Agent.CharContext.charSummaryWithJournal' inside
--   its own already-opened character-branch scope), @Just commit@ for a
--   caller crossing into a *different* branch than the one its own
--   'BranchOp' scope is opened on (e.g.
--   'Storyteller.Context.DSL.Compile.journalDelta', reading a resolved
--   character branch's journal from within the main story branch's own
--   scope) -- the same "content-addressed, branch-agnostic" read
--   'Storage.Core.readAt' already gives any 'Storage.Core.StoreT'
--   computation, not something specific to this effect.
data JournalAccess (branch :: k) (m :: Type -> Type) a where
  JournalWindow :: Maybe Core.ObjectHash -> FilePath -> Int -> Int -> Int -> JournalAccess branch m [Text]

makeSem ''JournalAccess

runJournalAccess :: forall branch r a. Member (BranchOp branch) r => Sem (JournalAccess branch ': r) a -> Sem r a
runJournalAccess = interpret $ \case
  JournalWindow mCommit path lookback maxOut padding ->
    map Tick.ftMessage <$> runStorage @branch (at mCommit (Tick.recentAtomsOf path lookback maxOut padding))
  where
    at Nothing        action = action
    at (Just commit)  action = Core.readAt commit action

-- ---------------------------------------------------------------------------
-- Conversation
-- ---------------------------------------------------------------------------

-- | One turn of a file's own tick-history-as-conversation -- the
--   role-preserving counterpart to 'JournalWindow's plain text, and the
--   one effect here most likely to have a genuinely native (not shimmed)
--   implementation on a backend with no tick history at all: a
--   SillyTavern-style backend's own chat log already *is* a sequence of
--   prompt\/response turns, with no tick history anywhere in the picture.
data Turn = UserTurn Text | AssistantTurn Text
  deriving (Eq, Show)

data ConversationAccess (branch :: k) (m :: Type -> Type) a where
  ConversationTurns :: FilePath -> ConversationAccess branch m [Turn]

makeSem ''ConversationAccess

runConversationAccess :: forall branch r a. Member (BranchOp branch) r => Sem (ConversationAccess branch ': r) a -> Sem r a
runConversationAccess = interpret $ \case
  ConversationTurns path -> toTurns <$> runStorage @branch (Tick.fileTicksOf path)
  where
    toTurns = concatMap turnOf . filter (not . isHidden)
    isHidden ft = lookup "hide" (Tick.ftFields ft) == Just "true"
    turnOf ft = case Tick.ftKind ft of
      "prompt" -> [UserTurn (Tick.ftMessage ft)]
      "atom"   -> [AssistantTurn (maybe (Tick.ftMessage ft) id (Tick.ftContent ft))]
      _        -> []

-- ---------------------------------------------------------------------------
-- Tracked-file policy
-- ---------------------------------------------------------------------------

-- | Which of @paths@ are atom-tracked (i.e. not a binary asset that opted
--   out of tracking) -- a policy question over history, separate from
--   reading any of the tracked files themselves. Tick-history dependent.
data TrackedFiles (branch :: k) (m :: Type -> Type) a where
  AtomTrackedAmong :: [FilePath] -> TrackedFiles branch m (Set FilePath)

makeSem ''TrackedFiles

runTrackedFiles :: forall branch r a. Member (BranchOp branch) r => Sem (TrackedFiles branch ': r) a -> Sem r a
runTrackedFiles = interpret $ \case
  AtomTrackedAmong paths -> runStorage @branch (Query.atomTrackedAmong paths)

-- ---------------------------------------------------------------------------
-- Tasks sync tracking
-- ---------------------------------------------------------------------------

-- | @tasks.md@'s own last-synced marker (see
--   'Storyteller.Writer.Agent.Tasks' for the full sync\/suggest
--   machinery this one ref drives) -- the newest atom on @tasksPath@
--   carrying a ref, scoped to the file's current lifetime. Genuinely
--   history-dependent (walks tick history to find it), and specific
--   enough to tasks.md's own convention that it doesn't belong lumped in
--   with 'JournalAccess' or 'ConversationAccess', even though all three
--   happen to read tick history on today's git-backed interpreter.
data TasksSyncTracking (branch :: k) (m :: Type -> Type) a where
  LastSyncedTasksRef :: FilePath -> TasksSyncTracking branch m (Maybe Core.ObjectHash)

makeSem ''TasksSyncTracking

runTasksSyncTracking :: forall branch r a. Member (BranchOp branch) r => Sem (TasksSyncTracking branch ': r) a -> Sem r a
runTasksSyncTracking = interpret $ \case
  LastSyncedTasksRef tasksPath -> runStorage @branch $ do
    ticks <- Tick.fileTicksOf tasksPath
    return $ case [ Core.ObjectHash r | ft <- reverse ticks, (r : _) <- [Tick.ftRefs ft] ] of
      (r : _) -> Just r
      []      -> Nothing

-- ---------------------------------------------------------------------------
-- Committing content
-- ---------------------------------------------------------------------------

-- | The shared "commit this content" vocabulary -- one real concept (four
--   operations that all bottom out in writing an atom), not four
--   accidentally-grouped operations: every real write here already goes
--   through the same alternatives ("freeze behind a checkpoint," "replace
--   wholesale," "append fresh", "append fresh with a ref") a caller
--   composes explicitly, exactly the same shape
--   'Storyteller.Writer.Agent.Tasks.exchangeTasksFile' already had.
data AtomWrite (branch :: k) (m :: Type -> Type) a where
  CheckpointFile  :: FilePath -> AtomWrite branch m ()
  SaveFileAsNew   :: FilePath -> FilePath -> Text -> AtomWrite branch m ()
  AddAtom         :: FilePath -> Text -> AtomWrite branch m Core.ObjectHash
  AddAtomWithRefs :: [Core.ObjectHash] -> FilePath -> Text -> AtomWrite branch m Core.ObjectHash

makeSem ''AtomWrite

runAtomWrite :: forall branch r a. Member (BranchOp branch) r => Sem (AtomWrite branch ': r) a -> Sem r a
runAtomWrite = interpret $ \case
  CheckpointFile path            -> runStorage @branch (Ops.checkpointFile path)
  SaveFileAsNew old new content   -> runStorage @branch (Ops.saveFileAsNew old new content)
  AddAtom path content           -> runStorage @branch (Ops.addAtom path content)
  AddAtomWithRefs refs path content -> runStorage @branch (Ops.addAtomWithRefs refs path content)

-- ---------------------------------------------------------------------------
-- Branch resolution
-- ---------------------------------------------------------------------------

-- | Resolves a branch name to its current commit -- the one operation the
--   context DSL needs that isn't expressible via any of the effects above
--   (see "Storyteller.Context.DSL.Compile"'s own module Haddock): real
--   backends happen to provide both this and tree access, but nothing
--   here assumes that pairing, so it's its own effect. No @branch@
--   phantom -- unlike everything else here, this resolves a *name*
--   through 'Storyteller.Core.Storage.StoryStorage', which is already
--   project-global, not scoped to any one already-open branch.
data BranchResolve (m :: Type -> Type) a where
  ResolveBranch :: BranchName -> BranchResolve m (Maybe Core.ObjectHash)

makeSem ''BranchResolve

runBranchResolve :: Member StoryStorage r => Sem (BranchResolve ': r) a -> Sem r a
runBranchResolve = interpret $ \case
  ResolveBranch name -> fmap (Core.ObjectHash . unTickId . branchHead) <$> getBranch name

-- ---------------------------------------------------------------------------
-- The whole vocabulary, one branch, one backend
-- ---------------------------------------------------------------------------

-- | Every branch-scoped effect above, discharged at once against a single
--   git-backed branch -- the one thing a caller composing this onto its
--   own interpreter stack actually wants: "give me the whole vocabulary
--   for this branch," not seven individual lines to remember to keep in
--   sync. Composes directly onto an existing stack (no @runM@ inside,
--   same as every interpreter above), and can be applied more than once
--   at different @branch@ type applications within the same stack (see
--   the module Haddock) -- a different backend supplies its own
--   equivalent of this function, discharging whichever subset of the
--   seven effects it can honestly back. 'BranchResolve' isn't included --
--   it has no @branch@ of its own to be scoped to; wire it separately
--   (once, project-wide) via 'runBranchResolve'.
runContentEffectsGit
  :: forall branch r a
  .  Member (BranchOp branch) r
  => Sem ( TreeAccess branch ': Presence branch ': JournalAccess branch ': ConversationAccess branch
         ': TrackedFiles branch ': TasksSyncTracking branch ': AtomWrite branch ': r ) a
  -> Sem r a
runContentEffectsGit =
    runAtomWrite @branch
  . runTasksSyncTracking @branch
  . runTrackedFiles @branch
  . runConversationAccess @branch
  . runJournalAccess @branch
  . runPresence @branch
  . runTreeAccess @branch
