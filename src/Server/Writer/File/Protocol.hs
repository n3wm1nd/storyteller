{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Protocol for /branch/{name}/{path} connections.
--
-- Commands: intent-based file operations. The server executes them and
--           pushes the resulting state back as a FileUpdate. No read or
--           resync commands — reconnect triggers a full state push.
--           'AskCharacter' is the one exception: its answer lands on the
--           *character's* branch, not this file's, so a ref-move
--           notification here would never surface it — see its own
--           Haddock. Summary content (see 'Server.Writer.File.summaryTicksFor')
--           needs no such exception: it rides along in the ordinary
--           'FileUpdate' push as synthetic ticks instead.
-- Events:   FilePresent/FileAbsent on connect, FileUpdate after mutations,
--           plus AgentLog and FileError.
module Server.Writer.File.Protocol
  ( FileCommand(..)
  , FileEvent(..)
  , ContextItem(..)
  , AtBranch(..)
  , commandKind
  ) where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import Server.Core.Protocol (Update, withId)
import Storyteller.Writer.Agent.ContextFilter (PickerRule)

-- | A piece of pinned context the client attaches to a chat prompt — an
--   atom or annotation the user selected as reference material. 'ciContent'
--   is what the agent actually reads; 'ciTickId'/'ciKind' are for
--   traceability only. See SPEC-SELECTION-ANNOTATIONS.md.
data ContextItem = ContextItem
  { ciTickId  :: T.Text
  , ciKind    :: T.Text
  , ciContent :: T.Text
  } deriving (Show)

instance FromJSON ContextItem where
  parseJSON = withObject "ContextItem" $ \o ->
    ContextItem <$> o .: "tickId" <*> o .: "kind" <*> o .: "content"

-- | One entry of an 'At' command's @branches@ field: a connected branch
--   (e.g. a character's journal, see WRITER.md) to also wind back and
--   replay, at the position the client picked for it — 'frontend/src/lib/
--   wsHelpers.ts''s 'atRebase' sends one of these per active character,
--   defaulting to 'nearestJournalMarker' but user-overridable, since the
--   right position isn't reliably inferrable server-side (story time and a
--   character branch's own position aren't in lock-step — flashbacks,
--   retellings, etc.). See 'Storyteller.Core.Git.atGeneric'.
data AtBranch = AtBranch
  { atBranchName   :: T.Text
  , atBranchTickId :: T.Text
  } deriving (Show)

instance FromJSON AtBranch where
  parseJSON = withObject "AtBranch" $ \o ->
    AtBranch <$> o .: "branch" <*> o .: "tickId"

-- | Commands the client may send on a file connection.
--   Each is an intent — the server decides what ticks result.
--
--   The "chat.*" variants are the input bar's routable targets:
--   'ChatAppend' is the instant, non-LLM verbatim insert; 'ChatWriter' is
--   Writer (or FlowWriter, implicitly, when 'cwFlowTid' is set — the tick
--   that was HEAD when the user started typing, so the agent can judge
--   whether atoms generated since then are still provisional); 'ChatFixer'
--   edits specific existing atoms in place; 'ChatNote' is instant and
--   non-LLM like 'ChatAppend', attaching an annotation instead of content.
--
--   'CreateFile' introduces this path into the tree as its own tick, empty —
--   distinct from 'ChatAppend', which both creates (implicitly, on a
--   not-yet-tracked path) and appends content in one step. Fails on an
--   already-present path rather than truncating it — see
--   'Server.Core.File.createFile'.
data FileCommand
  = CreateFile { fcId :: Maybe T.Text }
  | ChatAppend { fcId :: Maybe T.Text, fcContent :: T.Text }
  | Delete     { fcId :: Maybe T.Text }
  -- | Rename this path's current lifetime to 'fcNewPath' -- a rebase, not
  --   a forward event (contrast with 'Delete') -- see
  --   'Storage.Ops.renameFile'. Fails if 'fcNewPath' already exists.
  | Rename     { fcId :: Maybe T.Text, fcNewPath :: T.Text }
  -- | Freeze this path's current lifetime and clone it in full onto a
  --   fresh one -- see 'Storage.Ops.checkpointFile'\/'Server.Core.File.
  --   checkpointFile'. From here on, an atom edit\/delete can only reach
  --   the new copies; nothing before this point is touched.
  | Checkpoint { fcId :: Maybe T.Text }
  | EditAtom   { fcId :: Maybe T.Text, fcTickId :: T.Text, fcContent :: T.Text }
  -- | Edit a chat 'Storyteller.Writer.Agent.Prompt' tick's text in place.
  --   Distinct from 'EditAtom': a prompt's message carries no filesystem
  --   footprint the way an atom's does, so it goes through
  --   'Server.Writer.File.editChatPrompt' \/ 'Storyteller.Core.StorageMonad.editTick'
  --   rather than 'Server.Core.File.editFileAtom'.
  | EditPrompt { fcId :: Maybe T.Text, fcTickId :: T.Text, fcContent :: T.Text }
  -- | Drop every tick in @fcTargets@ from the chain, in one transaction --
  --   'Server.Core.File.deleteFileAtoms' is generic over *any* tick kind,
  --   not just atoms: an annotation (note, prompt, summary occurrence,
  --   ask, image) is a real chain-position tick exactly like an atom is,
  --   and 'correctGroup' already relies on this to drop a stale prompt
  --   tick alongside the atoms it produced. Same @fcTargets@\/@"targets"@
  --   shape as 'MergeAtoms'\/'SplitAtoms'\/'HideAtoms' -- a batch-capable
  --   command always takes a list here, even for one target, rather than
  --   a separate singular command.
  | DeleteTick { fcId :: Maybe T.Text, fcTargets :: [T.Text] }
  | MoveAtom   { fcId :: Maybe T.Text, fcTickId :: T.Text, fcAfterTickId :: Maybe T.Text }
  -- | Merge a contiguous run of one file's atoms (@fcTargets@) into one. See
  --   'Storyteller.Core.Edit.mergeAtoms'.
  | MergeAtoms { fcId :: Maybe T.Text, fcTargets :: [T.Text] }
  -- | Re-run the splitter over each of @fcTargets@'s own content, in place.
  --   See 'Storyteller.Core.Edit.splitTick'.
  | SplitAtoms { fcId :: Maybe T.Text, fcTargets :: [T.Text] }
  -- | Hide (or unhide) @fcTargets@ from an agent's ambient context, in
  --   place -- the atoms stay in the file. See 'Storage.Ops.setAtomHidden'.
  | HideAtoms   { fcId :: Maybe T.Text, fcTargets :: [T.Text] }
  | UnhideAtoms { fcId :: Maybe T.Text, fcTargets :: [T.Text] }
  -- | 'fcContextLayout' is the client's bucket-picker ordering for this
  --   call's ambient context (empty means "no layout configured", falling
  --   back to the default alphabetical order — see
  --   'Storyteller.Writer.Agent.Continuation.gatherFileContext'). See the
  --   project's context-assembly design notes for the picker model this
  --   implements.
  --
  --   'fcCharacterLayouts' is the same picker model, one entry per active
  --   character branch the client has curated (branch name -> layout) —
  --   see 'Server.Writer.File.activeCharacterContext'. A branch absent from
  --   this map (the common case: nobody has opened that character's
  --   context UI) means "no override", which reads as today's fixed
  --   sheet-in/journal-out behavior, not "show nothing".
  | ChatWriter { fcId :: Maybe T.Text, fcPromptText :: T.Text, fcContext :: [ContextItem], fcContextLayout :: [PickerRule], fcFlowTid :: Maybe T.Text, fcCharacterLayouts :: Map.Map T.Text [PickerRule] }
  -- | Roleplay writer: every character present on this file (see
  --   'Storyteller.Writer.Presence.activeCharactersFor') is interrogated,
  --   in character, for what they'd do or say before one scene gets
  --   written and appended -- see 'Server.Writer.File.roleplayWriter'.
  --   'fcPromptText' may be empty (the orchestrator continues the scene
  --   naturally in that case). No @context@\/@contextLayout@\/character
  --   layouts here yet, unlike 'ChatWriter' -- the per-character context a
  --   roleplay turn reads is each interrogated character's own full branch,
  --   not a curated ambient slice, so there's nothing for those knobs to
  --   configure yet.
  | RoleplayWrite { fcId :: Maybe T.Text, fcPromptText :: T.Text }
  | ChatFixer  { fcId :: Maybe T.Text, fcPromptText :: T.Text, fcContext :: [ContextItem], fcTargets :: [T.Text] }
  -- | Discuss, don't write: send a message to the chat agent, which sees
  --   this file's own prior 'ChatConverse' exchanges as conversation
  --   history (see 'Server.Writer.File.chatConverse') plus every other
  --   branch file as background. The reply lands as a single atom, no
  --   splitter — chat turns aren't paragraph-split prose. No @context@/
  --   @targets@: a chat file has no atom-selection concept of its own.
  | ChatConverse { fcId :: Maybe T.Text, fcPromptText :: T.Text }
  -- | Regenerate a chat exchange's reply, keeping the old reply as a
  --   cycle-able alternate (see 'Storyteller.Common.Swipe') instead of
  --   discarding it -- unlike 'ChatConverse', the prompt tick
  --   ('fcPromptTickId') is edited in place rather than resent as a new
  --   one. See 'Server.Writer.File.chatConverseSwipe'.
  | ChatConverseSwipe { fcId :: Maybe T.Text, fcPromptTickId :: T.Text, fcAtomTickId :: T.Text, fcPromptText :: T.Text }
  -- | Rotate an atom's own alternates forward one step. Generic -- any
  --   atom, chat or prose -- see 'Server.Core.File.cycleAtomSwipe'.
  | CycleSwipe { fcId :: Maybe T.Text, fcTickId :: T.Text }
  -- | Regenerate the chapter (this file) to fit its beat sheet
  --   (@ch{N}.outline.md@ by convention), respecting 'fcPromptText' as the
  --   user's steer. 'fcByBeat' selects the beat-by-beat driver over the
  --   whole-chapter one. A reconciliation, not a wipe — see
  --   'Server.Writer.File.chatChapterRegen'.
  | ChatRegen  { fcId :: Maybe T.Text, fcPromptText :: T.Text, fcContext :: [ContextItem], fcByBeat :: Bool }
  -- | "Correct this": delete 'fcPromptTickId' and every atom in
  --   'fcTargets' (an instruction group's own prompt + generated output),
  --   then regenerate from 'fcPromptText' via 'chatWriter', rebased at
  --   'fcPromptTickId' -- all within this one command's own transaction.
  --   Replaces composing the same effect client-side from a 'DeleteTick'
  --   per tick followed by a separate 'ChatWriter': that took N+1 round
  --   trips (N distinct undo points, the group visibly vanishing atom by
  --   atom before generation even started) for what's actually one atomic
  --   edit. See 'Server.Writer.File.Dispatch's handler and
  --   'frontend/src/app/fileview.actions.ts''s 'correctAtom'.
  | CorrectGroup { fcId :: Maybe T.Text, fcPromptTickId :: T.Text, fcTargets :: [T.Text], fcPromptText :: T.Text, fcContext :: [ContextItem], fcContextLayout :: [PickerRule], fcCharacterLayouts :: Map.Map T.Text [PickerRule] }
  -- | Split this file (a whole-story outline, @outline.md@ by convention)
  --   into per-chapter beat sheets. No prompt or targets — the outline text is
  --   the whole input; the model decides the chapter breakdown. See
  --   'Server.Writer.File.chatSplitOutline'.
  | ChatOutline { fcId :: Maybe T.Text }
  -- | Summarize exactly this file -- unlike the branch-level "summarize"
  --   command (which runs a whole kind across every file that qualifies,
  --   see 'Server.Writer.Branch.summarize'), this never touches any
  --   other file, even one that's also stale. See
  --   'Server.Writer.File.summarizePath's own Haddock for why that's a
  --   real capability, not just a narrower version of the same thing.
  | SummarizeFile { fcId :: Maybe T.Text }
  -- | Create an empty summary occurrence to write into directly -- a
  --   distinct intent from 'SummarizeFile' (no LLM call at all), the same
  --   way 'CreateFile' is a distinct intent from an LLM-populated write,
  --   not a flag on it. Reuses 'SummarizeFile's own coverage-finding and
  --   cursor-positioning machinery underneath (see
  --   'Server.Writer.File.summarizePathManual') -- that sharing lives in
  --   the Haskell functions those two commands call, not in the wire
  --   shape.
  | CreateSummary { fcId :: Maybe T.Text }
  -- | Instant, non-LLM: attach a note to each of 'fcTargets', or (when
  --   empty) to the file's current HEAD tick.
  | ChatNote   { fcId :: Maybe T.Text, fcNoteText :: T.Text, fcTargets :: [T.Text] }
  -- | Attach an image already sitting in the branch (e.g. dragged in from
  --   the file tree) to this file's timeline, by asset path rather than raw
  --   bytes -- see 'Server.Core.File.referenceImage'. Distinct from the HTTP
  --   @$image@ upload endpoint ('Server.Writer.Branch.uploadImage'), which
  --   deposits new bytes; this just points at ones that already exist.
  | ReferenceImage { fcId :: Maybe T.Text, fcAssetPath :: T.Text, fcCaption :: T.Text }
  -- | Presence: a character (character/{id} branch) enters or leaves the
  --   scene on this file — recorded as a "presence" tick scoped to this
  --   file's own chain, not the whole branch. See WRITER.md.
  | EnterScene { fcId :: Maybe T.Text, fcCharacter :: T.Text }
  | LeaveScene { fcId :: Maybe T.Text, fcCharacter :: T.Text }
  -- | Ask @fcCharacter@ a question, answered from only their own branch --
  --   see 'Server.Writer.File.askCharacter'\/'Storyteller.Writer.Agent.
  --   AskCharacter.askCharacterAgent'. Unlike every other command here, the
  --   result isn't just a chain mutation another connection's ref-move
  --   notification would pick up (the answer lands on the *character's*
  --   branch, not this file's) -- so this is the one 'FileCommand' whose
  --   dispatch actually returns something (a 'CharacterAnswered' event),
  --   pushed straight back to the asking connection.
  | AskCharacter { fcId :: Maybe T.Text, fcCharacter :: T.Text, fcQuestion :: T.Text }
  -- | Run 'fcCommand' rebased at 'fcTickId': the chain is temporarily wound
  --   back to that tick, the filesystem set to its snapshot, the inner
  --   command executed there, then every later tick is replayed on top of
  --   whatever the inner command produced. Lets a client re-target any
  --   command at a historical point without a dedicated code path per
  --   command — see 'Storyteller.Core.Git.atGeneric'.
  --
  --   'fcBranches' additionally winds each named connected branch (e.g.
  --   an active character's journal) to its own given position *around*
  --   the whole thing — before 'fcCommand' runs, replayed after — so the
  --   command executes in a world where every chosen branch, opened
  --   anywhere or not, sits at its chosen point; see 'AtBranch' and
  --   'Server.Writer.File.Dispatch.atBranches'. Cross-branch ref fixup
  --   needs no per-branch handling at all anymore (the transaction
  --   boundary's cascade covers every branch). Empty when no connected
  --   branch has a position to pin.
  | At         { fcId :: Maybe T.Text, fcTickId :: T.Text, fcCommand :: FileCommand, fcBranches :: [AtBranch] }
  deriving (Show)

instance FromJSON FileCommand where
  parseJSON = withObject "FileCommand" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    i <- o .:? "id"
    case t of
      "file.create" -> pure (CreateFile i)
      "chat.append" -> ChatAppend i <$> o .: "content"
      "delete"      -> pure (Delete i)
      "rename"      -> Rename i <$> o .: "newPath"
      "checkpoint"  -> pure (Checkpoint i)
      "edit.atom"   -> EditAtom   i <$> o .: "tickId" <*> o .: "content"
      "edit.prompt" -> EditPrompt i <$> o .: "tickId" <*> o .: "content"
      "delete.ticks" -> DeleteTick i . fromMaybe [] <$> o .:? "targets"
      "move.atom"   -> MoveAtom   i <$> o .: "tickId" <*> o .:? "afterTickId"
      "merge.atoms" -> MergeAtoms i . fromMaybe [] <$> o .:? "targets"
      "split.atoms" -> SplitAtoms i . fromMaybe [] <$> o .:? "targets"
      "hide.atoms"   -> HideAtoms   i . fromMaybe [] <$> o .:? "targets"
      "unhide.atoms" -> UnhideAtoms i . fromMaybe [] <$> o .:? "targets"
      "chat.writer" -> do
        context      <- fromMaybe [] <$> o .:? "context"
        layout       <- fromMaybe [] <$> o .:? "contextLayout"
        flowTid      <- o .:? "flowTid"
        charLayouts  <- fromMaybe Map.empty <$> o .:? "characterLayouts"
        ChatWriter i <$> o .: "text" <*> pure context <*> pure layout <*> pure flowTid <*> pure charLayouts
      "chat.roleplay" -> RoleplayWrite i . fromMaybe "" <$> o .:? "text"
      "chat.fixer"  -> do
        context <- fromMaybe [] <$> o .:? "context"
        targets <- fromMaybe [] <$> o .:? "targets"
        ChatFixer i <$> o .: "text" <*> pure context <*> pure targets
      "chat.regen"  -> do
        context <- fromMaybe [] <$> o .:? "context"
        byBeat  <- fromMaybe False <$> o .:? "byBeat"
        ChatRegen i <$> o .: "text" <*> pure context <*> pure byBeat
      "correct.group" -> do
        context     <- fromMaybe [] <$> o .:? "context"
        layout      <- fromMaybe [] <$> o .:? "contextLayout"
        charLayouts <- fromMaybe Map.empty <$> o .:? "characterLayouts"
        targets     <- fromMaybe [] <$> o .:? "targets"
        CorrectGroup i <$> o .: "promptTickId" <*> pure targets <*> o .: "text" <*> pure context <*> pure layout <*> pure charLayouts
      "chat.converse" -> ChatConverse i <$> o .: "text"
      "chat.converse.regen" ->
        ChatConverseSwipe i <$> o .: "promptTickId" <*> o .: "atomTickId" <*> o .: "text"
      "atom.swipe.cycle" -> CycleSwipe i <$> o .: "tickId"
      "chat.outline" -> pure (ChatOutline i)
      "summarize.file" -> pure (SummarizeFile i)
      "summarize.create" -> pure (CreateSummary i)
      "chat.note"   -> do
        targets <- fromMaybe [] <$> o .:? "targets"
        ChatNote i <$> o .: "text" <*> pure targets
      "reference.image" -> do
        caption <- fromMaybe "" <$> o .:? "caption"
        ReferenceImage i <$> o .: "asset" <*> pure caption
      "enter.scene" -> EnterScene i <$> o .: "character"
      "leave.scene" -> LeaveScene i <$> o .: "character"
      "ask.character" -> AskCharacter i <$> o .: "character" <*> o .: "question"
      "at"          -> At         i <$> o .: "tickId" <*> o .: "command" <*> (fromMaybe [] <$> o .:? "branches")
      _             -> fail ("unknown file command: " <> T.unpack t)

-- | Short label for logging — the same tag 'FromJSON' parses from, so it
--   matches the wire protocol's own vocabulary (see WS-PROTOCOL.md) rather
--   than leaking Haskell constructor names into logs. 'At' reports its
--   inner command's own kind alongside its own, since that's the
--   operationally interesting one.
commandKind :: FileCommand -> T.Text
commandKind = \case
  CreateFile {}   -> "file.create"
  ChatAppend {}   -> "chat.append"
  Delete {}       -> "delete"
  Rename {}       -> "rename"
  Checkpoint {}   -> "checkpoint"
  EditAtom {}     -> "edit.atom"
  EditPrompt {}   -> "edit.prompt"
  DeleteTick {}   -> "delete.ticks"
  MoveAtom {}     -> "move.atom"
  MergeAtoms {}   -> "merge.atoms"
  SplitAtoms {}   -> "split.atoms"
  HideAtoms {}    -> "hide.atoms"
  UnhideAtoms {}  -> "unhide.atoms"
  ChatWriter {}   -> "chat.writer"
  RoleplayWrite {} -> "chat.roleplay"
  ChatFixer {}    -> "chat.fixer"
  ChatRegen {}    -> "chat.regen"
  CorrectGroup {} -> "correct.group"
  ChatConverse {} -> "chat.converse"
  ChatConverseSwipe {} -> "chat.converse.regen"
  CycleSwipe {}   -> "atom.swipe.cycle"
  ChatOutline {}  -> "chat.outline"
  SummarizeFile {} -> "summarize.file"
  CreateSummary {} -> "summarize.create"
  ChatNote {}     -> "chat.note"
  ReferenceImage {} -> "reference.image"
  EnterScene {}   -> "enter.scene"
  LeaveScene {}   -> "leave.scene"
  AskCharacter {} -> "ask.character"
  At _ _ inner _  -> "at:" <> commandKind inner

-- | Events the server sends on a file connection.
--
--   FilePresent:  sent once on connect; the file exists, full tick state follows
--                 immediately as a FileUpdate.
--   FileAbsent:   sent once on connect; the file does not exist yet.
--   FileUpdate:   tick state push — upsert all ticks, set head to updateHead.
--   TickRemap:    a rebase/replace/move rewrote tick ids; [(from, to)] pairs.
--                 The client checks any tickId it's tracking locally (a
--                 rebase marker, a context selection) and updates it in
--                 place — a no-op if it isn't tracking any of them.
--   AgentLog:     progress message from a running agent.
--   FileError:    something went wrong; message is human-readable.
--
--   Not modeled here: "chat.preview.start"/"chat.preview"/"chat.preview.thinking"/
--   "chat.preview.end", the ephemeral LLM streaming preview. Those are pushed
--   directly by 'Server.Writer.Run.streamChunksWS', which is installed once
--   per connection (file or branch alike) around the whole command loop
--   rather than constructed by any 'Server.Writer.File'/'Server.Writer.Branch'
--   handler — the wire
--   shape is identical for both connection types, so there's nothing
--   File-specific for a 'FileEvent' constructor to add. See WS-PROTOCOL.md.
data FileEvent
  = FilePresent { feId :: Maybe T.Text }
  | FileAbsent  { feId :: Maybe T.Text }
  | FileUpdate  Update
  | TickRemap   [(T.Text, T.Text)]
  | AgentLog    { feLevel :: T.Text, feMessage :: T.Text }
  | FileError   T.Text
  -- | The answer to an 'AskCharacter' command, pushed straight back to the
  --   connection that asked -- see that constructor's own Haddock for why
  --   this is the one dispatch result that needs its own push rather than
  --   relying on a ref-move notification.
  | CharacterAnswered { feId :: Maybe T.Text, feCharacter :: T.Text, feQuestion :: T.Text, feAnswer :: T.Text }
  deriving (Show)

instance ToJSON FileEvent where
  toJSON = \case
    FilePresent mid ->
      object $ withId mid [ "type" .= ("file.present" :: T.Text) ]
    FileAbsent mid ->
      object $ withId mid [ "type" .= ("file.absent"  :: T.Text) ]
    FileUpdate u -> toJSON u
    TickRemap mapping ->
      object [ "type" .= ("tick.remap" :: T.Text), "mapping" .= mapping ]
    AgentLog level msg ->
      object [ "type"    .= ("agent.log" :: T.Text)
             , "level"   .= level
             , "message" .= msg ]
    FileError msg ->
      object [ "type" .= ("error" :: T.Text), "message" .= msg ]
    CharacterAnswered mid character question answer ->
      object $ withId mid
        [ "type"      .= ("character.answered" :: T.Text)
        , "character" .= character
        , "question"  .= question
        , "answer"    .= answer ]
