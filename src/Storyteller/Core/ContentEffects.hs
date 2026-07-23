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
--   @readConversation@), not rewritten. @tasks.md@'s own sync bookkeeping
--   went a different way -- see "Storyteller.Writer.Agent.Tasks"'s own
--   internal @TasksSync@ effect instead, kept local to that module rather
--   than shared here, the same way "Storyteller.Writer.Agent.Summarizer"'s
--   internal @Summarization@ effect is: neither is something the DSL or
--   any other agent ever needs to reach into.
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
  , JournalCuration(..)
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
import qualified Data.Text as T
import Polysemy
import Polysemy.Fail (Fail)

import qualified Storage.Core as Core
import qualified Storage.Query as Query
import qualified Storage.Tick as Tick

import Storyteller.Core.Branch (BranchOp, runStorage)
import Storyteller.Core.Storage (StoryStorage, getBranch)
import Storyteller.Core.Types (Branch(..), BranchName(..), TickId(..))
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
--
--   NOTE (2026-07-23, discussed but not resolved further): deliberately
--   *not* folded into @Runix.FileSystem@'s own 'Runix.FileSystem.FileSystem'
--   \/'Runix.FileSystem.FileSystemRead' effects, even though both are
--   "list files, read a blob" in shape. The difference is where the
--   position comes from: 'Runix.FileSystem''s own @project@ is a
--   *type-level* phantom, fixed once when an interpreter is wired
--   (@runStoryFSGit \@branch@) -- right for "the one named branch I keep
--   reading from, always its current live state" (what
--   "Storyteller.Writer.Agent.Tasks"\/"Storyteller.Writer.Agent.CharContext"
--   want). 'TreeSnapshot'\/'ReadTreeBlob' take their position as a
--   *value*-level 'Core.ObjectHash' instead, because the DSL's own
--   cross-branch reads (@in (charname | branch): ...@,
--   'Storyteller.Context.DSL.Compile.journalDelta') only learn *which*
--   commit to read at from a 'BranchResolve' call moments earlier in the
--   same evaluation -- often a different commit on every loop iteration.
--   Minting a fresh phantom-tagged 'Runix.FileSystem.FileSystem'
--   interpreter per dynamically-resolved commit isn't expressible cleanly
--   (a Polysemy row is fixed at compile time), so this stays its own
--   effect rather than folding into the existing one.
--
--   FIXME (2026-07-23): 'BranchResolve' currently hands back a raw
--   'Core.ObjectHash', imported directly from "Storage.Core" -- meaning
--   this whole effect vocabulary still presupposes content-addressing
--   exists at all ("the position of a branch is a hash of its content"),
--   which a backend with no history (a plain directory, a SillyTavern
--   export) genuinely doesn't have. Should own an opaque position\/ref
--   type here instead (produced by 'BranchResolve', consumed by
--   'TreeSnapshot'\/'ReadTreeBlob'\/'JournalWindow'), with the git-backed
--   interpreter free to implement it *as* an 'Core.ObjectHash' internally
--   -- not expose that choice to callers.
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
--
--   The three curation numbers are a named 'JournalCuration' record, not
--   three positional 'Int's -- they're same-typed and easy to transpose by
--   accident, and a record makes both this constructor and every call site
--   self-documenting (@JournalCuration { lookback = ..., maxOut = ...,
--   padding = ... }@ reads as what it means, @5 3 1@ doesn't).
data JournalAccess (branch :: k) (m :: Type -> Type) a where
  JournalWindow :: Maybe Core.ObjectHash -> FilePath -> JournalCuration -> JournalAccess branch m [Text]

-- | See 'Storage.Tick.recentAtomsOf' for what each field actually curates.
data JournalCuration = JournalCuration
  { lookback :: Int
  , maxOut   :: Int
  , padding  :: Int
  } deriving (Eq, Show)

makeSem ''JournalAccess

runJournalAccess :: forall branch r a. Member (BranchOp branch) r => Sem (JournalAccess branch ': r) a -> Sem r a
runJournalAccess = interpret $ \case
  JournalWindow mCommit path curation ->
    map Tick.ftMessage <$> runStorage @branch (at mCommit (Tick.recentAtomsOf path (lookback curation) (maxOut curation) (padding curation)))
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
--
--   FIXME: returns a raw 'Core.ObjectHash' -- see 'TreeAccess''s own
--   Haddock for why that leaks a content-addressing assumption this
--   effect boundary shouldn't presuppose, and what to do about it
--   (an opaque position type owned here instead).
data BranchResolve (m :: Type -> Type) a where
  ResolveBranch :: BranchName -> BranchResolve m (Maybe Core.ObjectHash)

-- | The exported call every caller actually wants: a missing branch is
--   never something DSL/agent code has a useful next step for beyond
--   aborting (there is no legitimate "for each branch, if it doesn't
--   exist, do X" control flow anywhere this is used -- see
--   'runContentEffectsGit''s Haddock for the more general kind of
--   "iterate over branches" question, which is a different, not-yet-built
--   operation and not this one), so this collapses the constructor's raw
--   'Maybe' to 'Fail' by hand rather than via 'makeSem'. The raw
--   constructor stays available (as 'ResolveBranch', via 'send') for the
--   rare interceptor that does need to see the distinction.
resolveBranch :: Members '[BranchResolve, Fail] r => BranchName -> Sem r Core.ObjectHash
resolveBranch name = send (ResolveBranch name) >>= \case
  Just commit -> pure commit
  Nothing     -> fail ("branch not found: " <> T.unpack (unBranchName name))

runBranchResolve :: Member StoryStorage r => Sem (BranchResolve ': r) a -> Sem r a
runBranchResolve = interpret $ \case
  ResolveBranch name -> fmap (Core.ObjectHash . unTickId . branchHead) <$> getBranch name

-- ---------------------------------------------------------------------------
-- The whole vocabulary, one branch, one backend
-- ---------------------------------------------------------------------------

-- | Every branch-scoped effect above, discharged at once against a single
--   git-backed branch -- the one thing a caller composing this onto its
--   own interpreter stack actually wants: "give me the whole vocabulary
--   for this branch," not five individual lines to remember to keep in
--   sync. Composes directly onto an existing stack (no @runM@ inside,
--   same as every interpreter above), and can be applied more than once
--   at different @branch@ type applications within the same stack (see
--   the module Haddock) -- a different backend supplies its own
--   equivalent of this function, discharging whichever subset of the
--   five effects it can honestly back. 'BranchResolve' isn't included --
--   it has no @branch@ of its own to be scoped to; wire it separately
--   (once, project-wide) via 'runBranchResolve'.
runContentEffectsGit
  :: forall branch r a
  .  Member (BranchOp branch) r
  => Sem ( TreeAccess branch ': Presence branch ': JournalAccess branch ': ConversationAccess branch
         ': TrackedFiles branch ': r ) a
  -> Sem r a
runContentEffectsGit =
    runTrackedFiles @branch
  . runConversationAccess @branch
  . runJournalAccess @branch
  . runPresence @branch
  . runTreeAccess @branch
