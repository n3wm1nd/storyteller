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
  ( -- * Why there is no file-reading effect here
    --
    -- $treeaccess

    -- * Why there is no file-tick effect here either
    --
    -- $filetickswasnothere

    -- * Character presence (tick-history dependent)
    Presence(..)
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

    -- * Why there is no summarized-read effect here
    --
    -- $summarized

    -- * Branch name resolution
  , BranchResolve(..)
  , resolveBranch
  , runBranchResolve

    -- * The story's known cast (spans every character branch)
  , Cast(..)
  , CastMember(..)
  , knownCast
  , runCast
  ) where

import Data.Kind (Type)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Polysemy
import Polysemy.Fail (Fail)

import qualified Storage.Core as Core
import qualified Storage.Tick as Tick

import Storyteller.Core.Branch (BranchOp, Branches, runStorage, withBranch)
import Runix.FileSystem (FileSystemRead(..))
import Storyteller.Core.Git (BranchTag(..), runStoryFSRead)
import Storyteller.Core.Storage (StoryStorage, getBranch, listBranches)
import Storyteller.Core.Types (Branch(..), BranchName(..), TickId(..))
import qualified Storyteller.Writer.Presence as WriterPresence
import Storyteller.Writer.Types (Character(..))

-- $treeaccess
--
-- There used to be a @TreeAccess branch@ effect here: "current head", "list
-- a commit's readable paths", "read a blob by hash". It is gone, and what
-- replaced it is worth stating, because the shape it had is an easy one to
-- reach for again.
--
-- It was a storage primitive with a constructor around it. Listing files
-- and reading them is 'Runix.FileSystem.FileSystem' \/
-- 'Runix.FileSystem.FileSystemRead' -- a vocabulary that already existed,
-- that works against a real directory or a test filesystem just as well as
-- against a branch, and that nothing in this module needed to reinvent.
-- What made reinventing it look necessary was the /position/: the DSL
-- reads at a commit it resolves mid-evaluation, and those effects take
-- their position from an interpreter rather than an argument. So the
-- position was threaded through the effect instead -- and, because a
-- 'Storyteller.Context.DSL.Value.Value' is lazy, through every leaf of
-- every scope as well.
--
-- The actual answer was to enter, not to carry: a caller that needs another
-- branch says so ('Storyteller.Core.Branch.Branches'), enters it, and reads
-- through the ordinary filesystem effects inside. Nothing carries a hash,
-- and no signature outside an interpreter mentions one.
--
-- The cost of getting that wrong was not local. Because every scope was
-- built through @TreeAccess@, and every entry point built a scope,
-- @TreeAccess branch@ appeared on roughly fifty signatures across the DSL
-- -- including definitions that provably read nothing at all. A capability
-- that spreads that far is usually not describing a capability; it is
-- describing a call graph. See
-- 'Storyteller.Context.DSL.Compile.currentScope', now the only place in the
-- DSL that turns a capability into a scope, and
-- "Storyteller.Context.DSL.Library"'s own note on what fell out once it
-- did.

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

-- $filetickswasnothere
--
-- There used to be a @FileTicks branch@ effect here: "@path@'s own current
-- ticks," one constructor, whose interpreter was
-- @runStorage \@branch (Storage.Tick.fileTicksOf path)@. It is gone, and
-- why is worth recording, because on paper it looked like exactly the
-- reuse this module argues for -- three agents had each written that same
-- line by hand, so it got a name.
--
-- But a name is not an abstraction. Every one of its callers discharged it
-- at its own call site (@runFileTicks \@branch $ ...@), and the
-- interpreter needed 'BranchOp' @branch@ -- which those callers already
-- held. So @Member (FileTicks branch) r@ never appeared in a single
-- signature anywhere: no caller could be given a different interpreter,
-- because no caller ever named the capability. What it actually bought was
-- one more layer of ceremony over the line it replaced, in four places,
-- while four *other* sites ("Storyteller.Writer.Agent.Write",
-- "Storyteller.Writer.Presence", "Server.Writer.File",
-- "Server.Writer.Library") went on writing that line raw.
--
-- The test that distinguishes it from its neighbours is simple, and worth
-- applying before adding anything here: does some function's *type* name
-- this capability, leaving the choice of interpreter to whoever runs it?
-- 'Presence', 'JournalAccess' and 'ConversationAccess' all
-- pass (they are 'Member' constraints throughout
-- "Storyteller.Context.DSL.Compile", discharged per branch by
-- 'Storyteller.Core.Context.runContextValue'), and 'Cast' passes
-- ("Storyteller.Writer.Agent.PresenceTrack" holds it, @app/Presence.hs@
-- interprets it). An effect that fails it is a rename, and a rename is
-- cheaper as a function -- or, as here, as the one line it was hiding.

-- $summarized
--
-- There used to be a @Summarized branch@ effect here, one constructor
-- (@ReadSummarized kinds path@) whose interpreter was a single call to
-- what is now 'Storyteller.Writer.Agent.Summarizer.densest'. It is gone,
-- and the DSL holds 'Storyteller.Writer.Agent.Summarizer.SummaryQuery'
-- instead.
--
-- It failed the same test 'FileTicks' did, one step removed: it named a
-- capability that already had a name. Summaries are
-- "Storyteller.Writer.Agent.Summarizer"'s concept -- it is the module that
-- decides what a compression is and produces them -- so a second effect
-- here, existing only to forward one call into that module, made the DSL's
-- reads look independent of it while being a thin alias for it. Two doors
-- to one room, and the far door was the one that could actually be given
-- another backend.
--
-- The branch phantom that made it look like a peer of 'Presence' and
-- 'JournalAccess' turned out not to be needed either: 'SummaryQuery' is
-- interpreted per call by 'Storyteller.Core.Context.runContextValue',
-- exactly where those are, and that application site is what fixes the
-- branch.

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
--   FIXME: returns a raw 'Core.ObjectHash', which presupposes that a
--   position is a content address -- true of git, not of every backend a
--   'BranchResolve' interpreter might have. What callers actually do with
--   the result is hand it straight back as a position
--   ('Storyteller.Core.Snapshot.runSnapshotFS'), never inspect it, so an
--   opaque position type owned by this module would serve them all
--   identically and presuppose nothing.
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
-- The story's known cast
-- ---------------------------------------------------------------------------

-- | One member of the story's known cast: its branch identity and
--   @sheet.md@ verbatim (empty if the branch has none yet -- a character
--   branch created but not fleshed out is still a legitimate cast
--   member). Same "hand the raw sheet over, let the caller decide what to
--   do with it" shape 'Server.Writer.Character.characterState' already
--   gives the sidebar's own display -- this is the same read, just for
--   every character branch at once instead of one. No display name here
--   -- that's 'Storyteller.Writer.Branches.branchDisplayName' applied to
--   'cmBranch', a caller-side concern this module can't reach for itself
--   (see 'runCast's own Haddock on the import cycle that would create).
data CastMember = CastMember
  { cmBranch :: BranchName
  , cmSheet  :: Text
  } deriving (Show, Eq)

-- | "Which characters does this story know about, and what do their
--   sheets say" -- the operation 'runContentEffectsGit''s own Haddock
--   flags as the general "iterate over branches" gap 'BranchResolve'
--   doesn't cover. No @branch@ phantom, same reasoning as 'BranchResolve':
--   enumerating every @character/*@ branch is inherently project-global,
--   not scoped to one already-open branch.
data Cast (m :: Type -> Type) a where
  KnownCast :: Cast m [CastMember]

makeSem ''Cast

-- | Built on 'StoryStorage' (list every branch, filter to @character/*@)
--   plus 'Branches': each character's branch is entered
--   ('Storyteller.Core.Branch.withBranch') and its @sheet.md@ read through
--   the ordinary 'Runix.FileSystem' vocabulary, exactly as any other
--   file read in this codebase is.
--
--   __That read is not the cheapest possible one, on purpose.__
--   'Storyteller.Core.Git.runStoryFSRead' materializes the branch's whole
--   readable-content tree on entry -- references only, but the
--   atom-tracked filter behind it is a chain walk, so the cost grows with
--   that character's history and is paid once per cast member. A
--   positioned single-path read ('Storage.Core.readPathAt') would be O(path
--   depth) and history-independent.
--
--   The filesystem effects are the primary way file data is reached here,
--   and that is worth more than the difference. A function written against
--   'Runix.FileSystem.FileSystemRead' runs against a branch, a snapshot, a
--   real directory or a test filesystem without knowing which; a function
--   written against a positioned read primitive runs against exactly one
--   backend and drags that backend into its own signature. Paying a
--   bounded, references-only overhead to keep every reader portable is the
--   trade this codebase makes deliberately -- see
--   'Storyteller.Core.Snapshot's own note on the same choice.
--
--   What /would/ be worth revisiting is not the per-read cost but how many
--   times a scope gets opened at all: this opens one per cast member, and
--   callers above it sometimes re-enter branches an outer scope already
--   had. That is a structuring question about scope lifetimes, not an
--   argument for reaching past the filesystem effects.
--
--   The @character/@ prefix check below is inlined rather than reusing
--   'Storyteller.Writer.Branches.classifyBranch' (the one real place that
--   convention is owned) -- that module imports
--   'Storyteller.Core.Prompt', which imports 'Storyteller.Core.Runtime',
--   which imports this module, so pulling it in here would be a real
--   compile-time cycle, not just a style preference. Keep the two in sync
--   if the convention ever changes (see WRITER.md's "Branch naming").
runCast
  :: forall castBranch r a
  .  Members '[StoryStorage, Branches, Fail] r
  => Sem (Cast ': r) a -> Sem r a
runCast = interpret $ \case
  KnownCast -> do
    branches <- listBranches
    let charBranches = [ b | b <- branches, "character/" `T.isPrefixOf` unBranchName (branchName b) ]
    mapM toCastMember charBranches
  where
    toCastMember :: Branch -> Sem r CastMember
    toCastMember b = do
      let name = branchName b
      -- Enters each character's own branch and reads through the ordinary
      -- filesystem effect. The capability to do that ('Branches') lives
      -- here, in the interpreter, and stops here: a caller only ever holds
      -- 'Cast' and learns nothing about branches, which is what makes this
      -- effect worth having rather than exporting "list the character
      -- branches" and letting every caller open them itself.
      --
      -- The raw 'ReadFile' constructor rather than
      -- 'Runix.FileSystem.readFile', because a character branch with no
      -- sheet yet is a legitimate cast member (see 'CastMember') -- the
      -- miss is an empty answer here, not a 'Fail'.
      sheet <- withBranch @castBranch name $ runStoryFSRead @(BranchTag castBranch) @castBranch (BranchTag name) $
        either (const "") TE.decodeUtf8 <$> send @(FileSystemRead (BranchTag castBranch)) (ReadFile "sheet.md")
      pure CastMember
        { cmBranch = name
        , cmSheet  = sheet
        }
