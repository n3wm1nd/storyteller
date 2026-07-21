{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | The roleplay writer: rather than one call producing a scene directly
--   (the shape every other writer agent in this folder has), a present
--   character's action or line is something this module has to actually
--   go find out, not assume.
--
--   'roleplayAgent' is plain Haskell orchestration, not a tool-calling
--   loop: it iterates directly over every present character (an ordinary
--   'forM', not a tool the model has to remember to call) and, for each
--   one, makes two real function calls -- 'questionForCharacterAgent' to
--   choose what to ask them, then 'characterIntentAgent' to actually ask.
--   *Whether* a character gets interrogated is therefore unconditional,
--   guaranteed by the iteration itself, never left to a model's judgement;
--   *what* gets asked is the one genuinely agentic decision in that step.
--   Once every present character has answered, 'composeSceneAgent' writes
--   the finished scene from their answers in one more plain call. None of
--   this needs a tool at all -- there's nothing here for the model to
--   decide to invoke or skip.
--
--   'characterIntentAgent' is the one real subagent in this module -- not a
--   single structured call, but a full tool-calling loop, scoped to one
--   character's own branch via 'Storyteller.Core.Git.runBranchAndFS' and
--   primed with their full, uncurated context (sheet, whole journal,
--   everything else on their branch -- see
--   'Storyteller.Context.DSL.Library.characterSummaryOf's @"journalFull"@
--   bucket, read by 'askCharacter' before the branch is even opened; a
--   character's own branch, not any windowed ambient slice, is their only
--   source of what's going on, besides shared world lore, which the
--   caller's own scene context already carries separately). See
--   'characterTools' for its exact tool surface -- broad glob\/read\/write\/
--   edit access to its own branch
--   (excluding @sheet.md@, fixed, and @journal.md@, append-only through
--   dedicated tools), plus read-only access to shared lore. It can dig as
--   deep into its own branch and lore as it wants, or do neither and answer
--   immediately. Every one of these tools is plain 'Runix.FileSystem'
--   access -- the one place in this whole system that needs more is the
--   *post-scene* journal write (see 'Server.Writer.File.roleplayWriter'),
--   which carries a real cross-branch ref back to the scene's own atom
--   ('Storage.Ops.addAtomWithRefs') that a bare filesystem write has no way
--   to express; nothing here needs that. Its loop contains no turn-budget
--   logic at all -- no counter, no limit check, no denial
--   branch, just query and (for every tool call the response carries)
--   'UniversalLLM.Tools.executeToolCallFromList' directly, exactly as if
--   there were no budget. The budget is entirely
--   'Storyteller.Core.LLM.Interceptor.withToolCallBudget', wrapped once
--   around the whole loop: it intercepts 'Runix.LLM.QueryLLM', still lets
--   every query through to the real model, but once too many tool calls
--   have already been made, doesn't hand a further attempt back to the
--   loop at all -- it appends a denial and asks the model again itself,
--   right there, until it gets back something the loop can execute or a
--   plain answer. The loop only ever sees one clean response per call; it
--   has no way to tell whether a denial round happened underneath it. A
--   model that keeps trying past a second, harder cap makes the whole call
--   fail outright rather than loop forever.
--
--   A scene's outcome can only be written up once it actually exists, so
--   'characterReflectAgent' -- one present character's own journal entry,
--   filtered to what they could plausibly perceive -- deliberately isn't
--   folded into 'characterIntentAgent': it's a separate, single-shot call a
--   caller runs once per active character *after* 'roleplayAgent' has
--   produced the finished scene (see 'Server.Writer.File.roleplayWriter'),
--   whether or not that character was ever asked anything.
--
--   Unlike most agents in this folder, 'roleplayAgent' and
--   'characterIntentAgent' can't be pure LLM cores -- reaching a character's
--   own branch at all is the whole point, same unavoidable exception
--   'Storyteller.Writer.Agent.CharContext' and
--   'Storyteller.Writer.Agent.Chat' already carve out for the same reason.
--   'questionForCharacterAgent', 'composeSceneAgent' and
--   'characterReflectAgent' have no such need and stay plain, effect-thin
--   calls like the rest of the folder.
module Storyteller.Writer.Agent.Roleplay
  ( roleplayAgent
  , characterIntentAgent
  , characterReflectAgent
  , characterOpeningMessages
  , reflectOpeningMessages
  ) where

import Control.Monad (forM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath (takeFileName)

import Autodocodec (HasCodec(..), dimapCodec)
import Polysemy
import Polysemy.Fail (Fail)
import qualified Runix.FileSystem as RFS
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, PathFilter(..), filterWrite, listAllFiles)
import Runix.Git (Git)
import Runix.LLM (queryLLM)
import Runix.LLM.ToolExecution (executeTool)
import Runix.Logging (Logging, info)
import qualified Runix.Tools as Tools
import UniversalLLM (Message(..), ModelConfig(..), getToolCallName)
import UniversalLLM.Tools (ToolParameter(..), LLMTool(..), mkToolWithMeta, llmToolToDefinition)

import Storyteller.Core.Git (BranchOp, BranchTag, runBranchAndFS)
import Storyteller.Core.Context (ContextStorage, resolveContext1, runContextValue)
import qualified Storyteller.Context.DSL.Library as CtxLibrary
import Storyteller.Context.DSL.Rendering (renderMessages)
import Storyteller.Core.LLM.Interceptor (withToolCallBudget)
import Storyteller.Core.LLM.Role (LLMs, AgentModel, ProseModel)
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getConfigWithPrompt, getPrompt)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Storage (StoryStorage)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Agent (CharContextBlock(..), CharLabel(..), CharSummary(..), Prose(..))
import Storyteller.Writer.Agent.Context (WorldContext(..))
import Storyteller.Writer.Branches (branchDisplayName)
import Storyteller.Writer.Lore (isLoreEligible)
import Storyteller.Writer.Types (Character(..))

-- | A phantom tag for opening one present character's branch filesystem at
--   a time, dynamically -- same role 'Server.Writer.File.ActiveChar' plays
--   there, just local to this module (the two aren't the same type; nothing
--   outside either module needs to name either tag).
data RoleplayChar

-- ---------------------------------------------------------------------------
-- Orchestration: plain Haskell, not a tool-calling loop
-- ---------------------------------------------------------------------------

-- | One question, one answer, per present character -- a label for display
--   and prompt-building.
type Exchange = (Text, Text, Text)

-- | Direct one beat of a scene. See the module Haddock: this is a plain
--   iteration over @characters@, not a tool the model chooses to call, so
--   every present character is guaranteed to be asked something -- what
--   gets asked, and how their answers turn into a finished scene, is the
--   model's job; whether they get asked at all isn't.
roleplayAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, BranchOp Main, Git, StoryStorage, ContextStorage, FileSystem (BranchTag Main), FileSystemRead (BranchTag Main), Fail, Logging] r)
  => WorldContext               -- ^ scene context: existing prose, world lore -- rendered into a concrete model's own messages only right before each call actually reaches 'queryLLM' (see 'Storyteller.Writer.Agent.Continuation.proseAgent's own Haddock on why upstream binding is wrong)
  -> [(CharLabel, Character)]  -- ^ every character present
  -> Text                      -- ^ the author's direction; may be empty
  -> Sem r Prose
roleplayAgent sceneContext characters prompt = do
  let roster = [ label | (CharLabel label, _) <- characters ]
  exchanges <- forM characters $ \(CharLabel label, character) -> do
    question <- questionForCharacterAgent sceneContext roster label prompt
    answer   <- askCharacter character label sceneContext question
    pure (label, question, answer)
  Prose <$> composeSceneAgent sceneContext exchanges prompt

-- | Read @character@'s own full context via the Context DSL --
--   @context.character@ (a branch override on the @contexts@ branch, then
--   'Storyteller.Context.DSL.Library.contextCharacterDefault' as fallback
--   -- see 'Storyteller.Core.Context.resolveContextQuery'), its
--   @"journalFull"@ bucket -- everything, uncurated, same as this always
--   wanted -- then open their branch just for 'characterIntentAgent''s own
--   tool loop (its @write_file@\/@edit_file@\/@add_thought@\/
--   @add_suspicion@ tools genuinely need that branch's write effects --
--   the context read itself doesn't, since the DSL crosses to it itself).
--   This is the one place this module actually reaches outside the
--   ambient scene context, which is why it (unlike
--   'questionForCharacterAgent'\/'composeSceneAgent') needs
--   'BranchOp Main'\/'Git'\/'StoryStorage'\/'ContextStorage' at all. Logs
--   the question and the answer it got back, not just that a call
--   happened -- with 'characterIntentAgent' potentially running several
--   turns internally, the question/answer pair is the one thing worth
--   seeing in the log even when nothing else is.
askCharacter
  :: forall r
  .  (LLMs r, Members '[PromptStorage, BranchOp Main, Git, StoryStorage, ContextStorage, FileSystem (BranchTag Main), FileSystemRead (BranchTag Main), Fail, Logging] r)
  => Character -> Text -> WorldContext -> Text -> Sem r Text
askCharacter (Character (BranchName branchName)) name sceneContext question = do
  info ("ask " <> name <> ": " <> question)
  let ident = branchDisplayName branchName
  charVal    <- resolveContext1 @Main "context.character" CtxLibrary.contextCharacterDefault ident
  ownContext <- runContextValue @Main (CtxLibrary.characterSummaryOf "journalFull" charVal)
  answer <- runBranchAndFS @RoleplayChar (BranchName branchName) $
    characterIntentAgent @(BranchTag RoleplayChar) name ownContext sceneContext question
  info (name <> " answers: " <> answer)
  pure answer

-- | Choose what to ask @label@ -- the one genuinely agentic decision in the
--   interrogation phase (see the module Haddock). A plain prose call: no
--   tools, nothing to look up, just a judgement call given the scene and
--   the author's direction.
questionForCharacterAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => WorldContext -> [Text] -> Text -> Text -> Sem r Text
questionForCharacterAgent (WorldContext ctx) roster label prompt = do
  configsWithPrompt <- getConfigWithPrompt "agent.roleplay.question" defaultQuestionSystemPrompt defaultQuestionConfig
  let contextMsgs = renderMessages ctx
  response <- queryLLM configsWithPrompt (contextMsgs ++ [UserText (renderQuestionTrailing roster label prompt)])
  pure (T.strip (mconcat [t | AssistantText t <- response]))

-- | Everything after @sceneContext@'s own (now real, separate) messages --
--   genuinely new/synthesized each call, never DSL conversational
--   structure, so flattening it into one trailing message loses nothing.
renderQuestionTrailing :: [Text] -> Text -> Text -> Text
renderQuestionTrailing roster label prompt =
  T.intercalate "\n\n" [rosterLine, direction, ask]
  where
    rosterLine = "Characters present in this scene: " <> T.intercalate ", " roster
    direction
      | T.null (T.strip prompt) = "No specific direction was given -- the scene continues naturally from here."
      | otherwise                = "Direction from the author: " <> prompt
    ask = "What do you ask " <> label <> "?"

defaultQuestionSystemPrompt :: Prompt
defaultQuestionSystemPrompt = Prompt $ T.unlines
  [ "You are directing one beat of a scene in a collaborative story. You're about to ask one present"
  , "character what they'd do or say right now -- your only job here is deciding exactly what to ask"
  , "them: a short, specific question about their action, reaction, or line of dialogue in this"
  , "moment, grounded in the scene and the author's direction. Output only the question itself, one"
  , "or two sentences, nothing else -- no preamble, no quotation marks around it."
  ]

-- | Deliberately well above what a one- or two-sentence question needs --
--   see 'Storyteller.Writer.Agent.AskCharacter.defaultAskConfig's own
--   Haddock: a reasoning-capable model's thinking tokens draw from this
--   same budget before any answer text, and a cap sized only for the
--   visible answer leaves nothing for the answer once reasoning runs.
--   Anthropic's own thinking budget is @min 5000 (maxTokens \`div\` 2)@ (see
--   'UniversalLLM.Providers.Anthropic.anthropicReasoning'), so anything
--   below ~3000 here can't even reach that cap -- 5000 leaves 2500 for
--   thinking and 2500 for the answer, with real margin either way.
defaultQuestionConfig :: [ModelConfig ProseModel]
defaultQuestionConfig = [MaxTokens 5000, Temperature 0.8]

-- | Write the scene's continuation from every present character's own
--   answer -- a plain prose call, same shape as 'questionForCharacterAgent':
--   no tools, everything it needs is already in hand.
composeSceneAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => WorldContext -> [Exchange] -> Text -> Sem r Text
composeSceneAgent (WorldContext ctx) exchanges prompt = do
  configsWithPrompt <- getConfigWithPrompt "agent.roleplay" defaultComposeSystemPrompt defaultComposeConfig
  info "roleplayAgent: composing the scene..."
  let contextMsgs = renderMessages ctx
  response <- queryLLM configsWithPrompt (contextMsgs ++ [UserText (renderComposeTrailing exchanges prompt)])
  let narrative = T.strip (mconcat [t | AssistantText t <- response])
  info ("roleplayAgent: finished scene (" <> T.pack (show (T.length narrative)) <> " chars):\n" <> narrative)
  pure narrative

-- | Everything after @sceneContext@'s own (now real, separate) messages --
--   see 'renderQuestionTrailing's own Haddock for why flattening the rest
--   loses nothing.
renderComposeTrailing :: [Exchange] -> Text -> Text
renderComposeTrailing exchanges prompt =
  T.intercalate "\n\n" ([direction] ++ map renderExchange exchanges ++ [closing])
  where
    direction
      | T.null (T.strip prompt) = "No specific direction was given -- continue the scene naturally from here."
      | otherwise                = "Direction from the author: " <> prompt
    renderExchange (label, question, answer) =
      "Asked " <> label <> ": " <> question <> "\n\n" <> label <> "'s stated intent (their planned "
      <> "action, mood, and a few lines they might say -- material to write from, not a script to "
      <> "copy verbatim):\n\n" <> answer
    closing = "Write the scene's continuation now."

defaultComposeSystemPrompt :: Prompt
defaultComposeSystemPrompt = Prompt $ T.unlines
  [ "You are directing one beat of a scene in a collaborative story. You've already asked every"
  , "present character what they'd do, their mood, and a few lines they might say, each grounded in"
  , "only what they actually know -- write the scene's continuation as prose from that material: one"
  , "coherent narrative that folds every character's stated intent into what actually happens, moving"
  , "the story forward. Their stated lines are options, not a script -- use your own judgement for the"
  , "actual wording, blocking, and pacing; don't just concatenate what they gave you. Do not invent a"
  , "significant action or line of dialogue that isn't grounded in what a character actually stated."
  , "Output only the finished prose, nothing else."
  ]

-- | Same thinking-budget headroom reasoning as 'defaultQuestionConfig' --
--   the finished scene itself can run long, so this needs both the
--   thinking-model margin and genuine room for the prose.
defaultComposeConfig :: [ModelConfig ProseModel]
defaultComposeConfig = [MaxTokens 6000, Temperature 0.8]

-- ---------------------------------------------------------------------------
-- The character subagent
-- ---------------------------------------------------------------------------

-- | Answer, in character, what @name@ would do or say -- a full subagent
--   over their own branch, not a single structured call: it starts primed
--   with @ownContext@ (their sheet, their whole journal, and everything
--   else on their branch -- see the module Haddock on why this is a full
--   read, not a windowed slice), and can use any of 'characterTools' (own
--   branch read\/write\/edit, journal append, read-only lore) as it sees
--   fit before settling on an answer. It's free to ignore all of them and
--   answer immediately from @ownContext@ alone. The loop is
--   'Storyteller.Writer.Agent.Chat.chatAgent''s exact shape, reused rather
--   than redesigned: query, execute any tool calls, recurse; the first turn
--   with no calls has its text taken as the answer.
--
--   Its opening turn is a real @['Message']@, not one flattened 'UserText'
--   (see 'characterOpeningMessages') -- the same reasoning
--   'Storyteller.Writer.Agent.Write.buildChapterMessages' already applies to
--   chapter continuation: a scene beat re-derives and resends this whole
--   opening fresh every single call (there's no persisted conversation
--   across beats the way a chat history would give it), so whether a
--   provider's prompt cache can reuse anything at all depends entirely on
--   whether this call's opening messages are byte-identical, up to some
--   point, to a previous call's. @ownContext@'s 'csJournal' is the one part
--   guaranteed to differ every beat (it only ever grows -- see
--   'characterReflectAgent'); everything else here is comparatively stable.
--   Fusing all of it into one string, in any order, means a single changed
--   byte anywhere invalidates the whole thing; splitting it into separate
--   messages means only the messages at or after the change are ever paid
--   for again.
characterIntentAgent
  :: forall project r
  .  ( LLMs r
     , Members '[ FileSystem project, FileSystemRead project, FileSystemWrite project
                , FileSystem (BranchTag Main), FileSystemRead (BranchTag Main)
                , PromptStorage, Fail, Logging] r
     )
  => Text                  -- ^ this character's display name
  -> CharSummary            -- ^ their own full, uncurated branch context (see 'Storyteller.Context.DSL.Library.characterSummaryOf')
  -> WorldContext          -- ^ the scene's own context (existing prose, world lore)
  -> Text                  -- ^ the question put to them
  -> Sem r Text
characterIntentAgent name ownContext sceneContext question = do
  configsWithPrompt <- getConfigWithPrompt "agent.roleplay.character" defaultCharacterSystemPrompt defaultCharacterConfig
  lore <- loreFileList @r
  let tools = characterTools @project @(Fail ': r)
      configsWithTools = Tools (map llmToolToDefinition tools) : configsWithPrompt
      opening = characterOpeningMessages name ownContext sceneContext lore question
  withToolCallBudget @AgentModel toolCallSoftLimit toolCallHardRounds
    (go tools configsWithTools opening)
  where
    -- Room for a couple of real look-arounds before 'withToolCallBudget'
    -- starts transparently denying them -- 'go' below never sees that
    -- decision, only ever a clean response.
    toolCallSoftLimit  = 6 :: Int
    toolCallHardRounds = 3 :: Int

    go tools configsWithTools history = do
      info $ "characterIntentAgent(" <> name <> "): querying model..."
      response <- queryLLM configsWithTools history
      case [tc | AssistantTool tc <- response] of
        [] -> pure (T.strip (mconcat [t | AssistantText t <- response]))
        calls -> do
          results <- forM calls $ \tc -> do
            info $ "characterIntentAgent(" <> name <> "): calling " <> getToolCallName tc
            executeTool tools tc
          go tools configsWithTools (history ++ response ++ map ToolResultMsg results)

-- | The character subagent's window into its own branch, plus read-only
--   access to shared world lore. Own-branch access is broad -- glob, read,
--   write, and edit any file the character wants to create or maintain (a
--   per-character notes file under @characters/*.md@ is the encouraged
--   convention -- see 'characterIdentityNote' -- but nothing here enforces
--   that layout structurally) -- with exactly two paths carved out:
--   @sheet.md@ (fixed identity, never written here) and @journal.md@
--   (append-only, through 'addThoughtTool'\/'addSuspicionTool' below, never
--   a direct overwrite or edit).
--
--   That carve-out is enforced at the filesystem-*effect* layer
--   ('protectCharacterFiles', baked directly into @write_file@\/@edit_file@'s
--   own tool definitions below), not by checking the path inside their
--   implementation -- the same 'Runix.FileSystem.PathFilter'\/
--   'Runix.FileSystem.filterWrite' machinery
--   'Storyteller.Writer.Agent.ContextFilter.hideBinaryFiles' already uses
--   for read-side narrowing, just on the write side. Wrapping
--   only those two tools' own functions (rather than the whole tool loop)
--   is deliberate: @add_thought@\/@add_suspicion@ append to @journal.md@
--   through this exact same 'FileSystemWrite' effect, and would be denied
--   right alongside a deliberate overwrite attempt if the filter reached
--   them too.
--
--   Lore access ('loreGlobTool'\/'loreReadFileTool') is deliberately the
--   read-only counterpart: shared world lore is common ground truth, not
--   something any one character privately misremembers -- see the design
--   note this settled on. It has its own tool names (@lore_glob@\/
--   @lore_read_file@) rather than reusing @glob@\/@read_file@, which would
--   otherwise collide -- an LLM tool list can't have two tools sharing a
--   name, and it also gives the model an unambiguous way to say which
--   filesystem it means.
characterTools
  :: forall project r
  .  Members '[ FileSystem project, FileSystemRead project, FileSystemWrite project
              , FileSystem (BranchTag Main), FileSystemRead (BranchTag Main), Fail] r
  => [LLMTool (Sem r)]
characterTools =
  [ LLMTool (mkToolWithMeta
      "write_file"
      "Create or overwrite a file on your own branch. This call itself is invisible to everyone else \
      \-- don't mention in your actual answer whether or how you used it, just call it (or don't) and \
      \move on."
      (\path content -> protectCharacterFiles @project (Tools.writeFile @project path content))
      "path" "path to create or overwrite, e.g. characters/owen.md"
      "content" "the full new content for this file")
  , LLMTool (mkToolWithMeta
      "edit_file"
      "Replace one exact occurrence of old_string with new_string in a file on your own branch. This \
      \call itself is invisible to everyone else -- don't mention in your actual answer whether or how \
      \you used it, just call it (or don't) and move on."
      (\path old new -> protectCharacterFiles @project (Tools.editFile @project path old new))
      "path" "path of the file to edit"
      "old_string" "the exact text to replace -- must appear exactly once in the file"
      "new_string" "the text to replace it with")
  , addThoughtTool @project
  , addSuspicionTool @project
  , LLMTool (mkToolWithMeta
      "lore_glob"
      "Find shared world-lore files by glob pattern (e.g. \"**/*\" for everything). Read-only -- this \
      \is common ground truth, not yours to change."
      (Tools.glob @(BranchTag Main))
      "pattern" "glob pattern to match lore file paths against")
  , LLMTool (mkToolWithMeta
      "lore_read_file"
      "Read a shared world-lore file's content by exact path. Read-only."
      (Tools.readFile @(BranchTag Main))
      "path" "exact path of the lore file to read"
      "offset" "optional 1-based line number to start from"
      "limit" "optional number of lines to return")
  ]

-- | Wrap @action@ so @sheet.md@\/@journal.md@ can never be written on this
--   character's own branch through it -- see 'characterTools's own Haddock
--   for why this is a real filesystem-effect interception, not a per-tool
--   guess. Same 'Runix.FileSystem.PathFilter'\/'Runix.FileSystem.filterWrite'
--   machinery 'Storyteller.Writer.Agent.ContextFilter.hideBinaryFiles'
--   already uses for read-side narrowing, on the write side instead.
protectCharacterFiles
  :: forall project r a
  .  Members '[FileSystem project, FileSystemWrite project] r
  => Sem r a -> Sem r a
protectCharacterFiles = filterWrite @project filt
  where
    filt = PathFilter
      { shouldInclude = \p -> takeFileName p `notElem` ["sheet.md", "journal.md"]
      , filterName = "sheet.md and journal.md are not directly writable"
      }

-- | Append one line to @journal.md@, creating it if it doesn't exist yet --
--   plain 'Runix.FileSystem' reads\/writes, same layer every other tool in
--   this module works at. Deliberately not a generic write\/edit (see
--   'characterTools's own Haddock): every call only ever grows the file,
--   never rewrites or removes anything already there.
appendJournalLine
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, FileSystemWrite project, Fail] r
  => Text -> Sem r ()
appendJournalLine line = do
  exists <- RFS.fileExists @project "journal.md"
  existing <- if exists then TE.decodeUtf8 <$> RFS.readFile @project "journal.md" else pure ""
  let sep = if T.null (T.strip existing) then "" else "\n\n"
  RFS.writeFile @project "journal.md" (TE.encodeUtf8 (existing <> sep <> line))

newtype ThoughtText = ThoughtText Text
instance HasCodec ThoughtText where
  codec = dimapCodec ThoughtText (\(ThoughtText t) -> t) codec
instance ToolParameter ThoughtText where
  paramName = "text"
  paramDescription = "the thought to record"

addThought :: forall project r. Members '[FileSystem project, FileSystemRead project, FileSystemWrite project, Fail] r => ThoughtText -> Sem r Text
addThought (ThoughtText text) = appendJournalLine @project text >> pure "recorded"

addThoughtTool :: forall project r. Members '[FileSystem project, FileSystemRead project, FileSystemWrite project, Fail] r => LLMTool (Sem r)
addThoughtTool = LLMTool $ mkToolWithMeta
  "add_thought"
  "Record a private thought or reflection in your own journal -- a real-time mental note, not a \
  \record of what actually happened (that gets written up separately, afterward). This call itself \
  \is invisible to everyone else -- don't mention in your actual answer whether or how you used it, \
  \just call it (or don't) and move on."
  (addThought @project)
  "text" "the thought to record"

newtype SuspicionText = SuspicionText Text
instance HasCodec SuspicionText where
  codec = dimapCodec SuspicionText (\(SuspicionText t) -> t) codec
instance ToolParameter SuspicionText where
  paramName = "text"
  paramDescription = "the suspicion to record"

addSuspicion :: forall project r. Members '[FileSystem project, FileSystemRead project, FileSystemWrite project, Fail] r => SuspicionText -> Sem r Text
addSuspicion (SuspicionText text) = appendJournalLine @project ("Suspicion: " <> text) >> pure "recorded"

addSuspicionTool :: forall project r. Members '[FileSystem project, FileSystemRead project, FileSystemWrite project, Fail] r => LLMTool (Sem r)
addSuspicionTool = LLMTool $ mkToolWithMeta
  "add_suspicion"
  "Record a suspicion about someone or something in your own journal -- a hunch you can't yet \
  \confirm, not an established fact. This call itself is invisible to everyone else -- don't mention \
  \in your actual answer whether or how you used it, just call it (or don't) and move on."
  (addSuspicion @project)
  "text" "the suspicion to record"

-- | @characterIntentAgent@'s opening turn as a real @['Message']@ -- see its
--   own Haddock for why this matters, not just how. Ordered stable to
--   volatile: identity, then @ownContext@'s three parts in ascending order
--   of how often each actually changes call to call (sheet -- never;
--   context/tasks/notes -- only when the model's own tools edit them; lore
--   list -- only when new lore is added; journal -- every single beat, via
--   'characterReflectAgent'), then whatever's genuinely new this call
--   (scene context, the question).
--
--   Each of the three 'CharSummary' parts (plus lore) is its own
--   @('UserText' label, 'AssistantText' content)@ pair, dropped entirely
--   when empty -- the same shape, for the same two reasons,
--   'Storyteller.Writer.Agent.Write.buildChapterMessages' already uses for
--   earlier chapters: framing established material as something this
--   character already has (accurate -- it's their own sheet, their own
--   notes, their own journal), and guaranteeing a role switch on both sides
--   of every pair regardless of whether a provider concatenates adjacent
--   same-role messages. That guarantee is what actually matters here: it's
--   what keeps a change to the always-changing journal from being able to
--   silently fuse backward into, and invalidate, the stable prefix in front
--   of it, no matter how a given provider handles same-role adjacency.
characterOpeningMessages :: Text -> CharSummary -> WorldContext -> [FilePath] -> Text -> [Message m]
characterOpeningMessages name ownContext (WorldContext ctx) lore question =
  UserText (characterIdentityNote name)
    : labelledPair "## Your character sheet" (blocksText (csSheet ownContext))
   ++ labelledPair "## What else I know" (T.intercalate "\n\n" (filter (not . T.null) [blocksText (csContext ownContext), renderLoreList lore]))
   ++ labelledPair "## My own journal so far" (blocksText (csJournal ownContext))
   ++ sceneMsgs
   ++ [UserText asked]
  where
    -- Real, separate messages -- not flattened into one string the way the
    -- rest of this function's static framing is -- so any role structure
    -- @sceneContext@ itself carries (@context.main@'s own alternating-turn
    -- "chapters" bucket, say) survives into this call too.
    sceneMessages = renderMessages ctx
    sceneMsgs
      | null sceneMessages = []
      | otherwise           = UserText "### The scene so far" : sceneMessages
    asked = "You're being asked: " <> question

-- | One labelled section as a @('UserText', 'AssistantText')@ pair -- see
--   'characterOpeningMessages'\' own Haddock for why both the framing and
--   the role switch matter. Dropped entirely (not sent as an empty pair)
--   when @content@ is blank, so an absent file never surfaces as a header
--   with nothing under it.
labelledPair :: Text -> Text -> [Message m]
labelledPair label content
  | T.null (T.strip content) = []
  | otherwise                 = [UserText label, AssistantText content]

blocksText :: [CharContextBlock] -> Text
blocksText = T.intercalate "\n\n" . map (\(CharContextBlock b) -> b)

-- | Every shared lore path currently on the Main branch -- given to a
--   character subagent as plain text up front (see 'renderLoreList'), so it
--   can call @lore_read_file@ directly on a path it already knows about
--   instead of spending a turn on @lore_glob@ just to find out what
--   exists. Cheap: only paths, never content -- lore text itself stays
--   genuinely on-demand, only the "what's out there" listing is
--   unconditional, the same two-tier split 'characterTools's own Haddock
--   draws between a character's own branch (fully injected) and lore
--   (fetched only if asked for).
loreFileList :: forall r. Members '[FileSystem (BranchTag Main), Fail] r => Sem r [FilePath]
loreFileList = filter isLoreEligible <$> listAllFiles @(BranchTag Main) "/"

renderLoreList :: [FilePath] -> Text
renderLoreList [] = "There is no shared world lore on file yet."
renderLoreList paths = T.unlines
  ( "### Shared world lore available (call lore_read_file on one you want; lore_glob only if you need to search further)"
  : map (\p -> "- " <> T.pack p) paths
  )

-- | The identity/framing block every 'characterIntentAgent' call opens
--   with -- same reasoning, and the same "fixed Haskell code in the user
--   message, never templated into the overridable system prompt default"
--   choice, as 'Storyteller.Writer.Agent.Tasks.tasksIdentityNote'.
characterIdentityNote :: Text -> Text
characterIdentityNote name = T.unlines
  [ "You are answering, in character, as " <> name <> " -- a fictional character in this story, not"
  , "its narrator or author. Everything above this line (once shown) is what " <> name <> " actually"
  , "knows: their own character sheet, their own whole journal, and anything else on their own"
  , "branch, shown to you in full already -- there's no need to go looking for any of it yourself."
  , "This, plus shared world lore (see below), is the only source of what's going on -- there is no"
  , "other ambient context here."
  , ""
  , "Your own branch is yours to maintain: write_file/edit_file create or update any file on it --"
  , "keeping a separate note file per character you know (e.g. characters/owen.md, for what you know"
  , "or believe about Owen specifically) is the encouraged way to track \"who do I know and what do I"
  , "know about them\", but nothing enforces that layout; organize it however actually helps. Since"
  , "everything already on your branch is shown above, edit_file's old_string can be matched straight"
  , "from what you see there. The two exceptions: sheet.md is fixed, not yours to write, and"
  , "journal.md only ever grows through add_thought/add_suspicion below -- never a direct write or"
  , "edit -- so nothing you or anyone else jotted down earlier can be silently rewritten."
  , ""
  , "add_thought records a private real-time reflection; add_suspicion records a hunch about someone"
  , "or something you can't yet confirm. Both are entirely optional -- use them only if they actually"
  , "help. Neither is a record of what actually happens in this scene; that gets written up separately"
  , "afterward, from everyone's stated intentions together, not by you."
  , ""
  , "lore_glob/lore_read_file give you read-only access to the story's shared world lore -- common"
  , "ground truth, not yours to change. The list of what's available is already given below; call"
  , "lore_read_file directly on whichever path is actually relevant rather than lore_glob first to"
  , "look for it (lore_glob is still there if you need to search for something not in that list)."
  , "Beyond your own branch, this lore, and what's given directly below, you know nothing about the"
  , "current situation -- if you don't already know a present character from your own branch, you"
  , "don't know them."
  , ""
  , "Ground everything below in only what " <> name <> " could plausibly know or perceive right now"
  , "-- never anything only the reader or another character would know."
  , ""
  , "You are informing the narrator, not performing the scene yourself -- don't write it as though"
  , "you were already speaking to someone or acting it out in the moment. Answer with exactly these"
  , "three sections:"
  , ""
  , "## What I'd do"
  , "A plain, concrete description of the action(s) " <> name <> " would take right now -- what"
  , "happens, not a performed first-person narration of it."
  , ""
  , "## Mood"
  , name <> "'s emotional state and overall tone right now, a phrase or two."
  , ""
  , "## Things I might say"
  , "2 to 4 different possible lines " <> name <> " might actually say -- real options for the"
  , "narrator to draw from or adapt, not a single scripted line and not a whole exchange with anyone"
  , "else."
  ]

defaultCharacterSystemPrompt :: Prompt
defaultCharacterSystemPrompt =
  "You are informing a story's narrator what one character would do, feel, and might say right now -- \
  \grounded strictly in what that character actually knows or could perceive, never anything only the \
  \reader or another character would know. You provide information for the narrator to write from, \
  \not a performed scene."

-- | Room for a reasoning-capable model's thinking tokens plus the full
--   three-section answer ('characterIdentityNote') -- see
--   'defaultQuestionConfig's own Haddock on why this needs real headroom,
--   not just what the visible answer looks like it needs. This path also
--   runs a tool loop ('characterTools'), so a low cap can starve the final
--   text-only turn of both thinking and answer room simultaneously.
defaultCharacterConfig :: [ModelConfig AgentModel]
defaultCharacterConfig = [MaxTokens 8000, Temperature 0.8]

-- ---------------------------------------------------------------------------
-- Post-scene reflection
-- ---------------------------------------------------------------------------

-- | One present character's own journal entry for a scene that's already
--   finished, and their chance to *act* on it: unlike 'characterIntentAgent'
--   (which only ever has a plan to go on), this runs after the real outcome
--   is known, so it's the one place a character can correct a
--   @characters/*.md@ note that turned out wrong, or record a thought
--   prompted by what actually happened rather than what they merely
--   expected -- see the module Haddock on why this is separate from, and
--   always runs after, 'characterIntentAgent'. A full tool loop, same
--   'characterTools' and the same shape as 'characterIntentAgent''s own --
--   query, execute any tool calls, recurse -- with the final turn's text
--   taken as the journal entry a caller commits, ref'd back to the scene's
--   own atom (see 'Server.Writer.File.roleplayWriter').
characterReflectAgent
  :: forall project r
  .  ( LLMs r
     , Members '[ FileSystem project, FileSystemRead project, FileSystemWrite project
                , FileSystem (BranchTag Main), FileSystemRead (BranchTag Main)
                , PromptStorage, Fail, Logging] r
     )
  => Text                 -- ^ this character's display name
  -> CharSummary          -- ^ their own pre-scene branch context (see 'Storyteller.Context.DSL.Library.characterSummaryOf')
  -> Text                 -- ^ the scene's finished narrative
  -> Sem r Text
characterReflectAgent name ownContext narrative = do
  configsWithPrompt <- getConfigWithPrompt "agent.roleplay.reflect" defaultReflectSystemPrompt defaultReflectConfig
  Prompt closing    <- getPrompt "agent.roleplay.reflect.instructions" defaultReflectInstructions
  lore <- loreFileList @r
  let tools = characterTools @project @(Fail ': r)
      configsWithTools = Tools (map llmToolToDefinition tools) : configsWithPrompt
  withToolCallBudget @AgentModel toolCallSoftLimit toolCallHardRounds
    (go tools configsWithTools (reflectOpeningMessages name ownContext narrative lore closing))
  where
    toolCallSoftLimit  = 6 :: Int
    toolCallHardRounds = 3 :: Int

    go tools configsWithTools history = do
      info $ "characterReflectAgent(" <> name <> "): querying model..."
      response <- queryLLM configsWithTools history
      case [tc | AssistantTool tc <- response] of
        [] ->
          let entry = T.strip (mconcat [t | AssistantText t <- response])
          in entry <$ info ("characterReflectAgent(" <> name <> "): entry: " <> entry)
        calls -> do
          results <- forM calls $ \tc -> do
            info $ "characterReflectAgent(" <> name <> "): calling " <> getToolCallName tc
            executeTool tools tc
          go tools configsWithTools (history ++ response ++ map ToolResultMsg results)

-- | 'characterReflectAgent''s opening turn as a real @['Message']@ -- same
--   shape and reasoning as 'characterOpeningMessages', reused directly
--   rather than redesigned: stable-to-volatile 'CharSummary' parts as
--   labelled pairs, then the one thing genuinely new this call (the scene's
--   finished narrative plus closing instructions) last.
reflectOpeningMessages :: Text -> CharSummary -> Text -> [FilePath] -> Text -> [Message m]
reflectOpeningMessages name ownContext narrative lore closing =
  UserText identity
    : labelledPair "## Your character sheet" (blocksText (csSheet ownContext))
   ++ labelledPair "## What else I know" (T.intercalate "\n\n" (filter (not . T.null) [blocksText (csContext ownContext), renderLoreList lore]))
   ++ labelledPair "## My own journal so far" (blocksText (csJournal ownContext))
   ++ [UserText (sceneBlock <> "\n\n" <> closing)]
  where
    identity = "This journal entry is " <> name <> "'s own private account -- write strictly from "
             <> name <> "'s own point of view, using their context above (if any) as what they"
             <> " already knew going in."
    sceneBlock = "What just happened in the scene:\n\n" <> narrative

defaultReflectSystemPrompt :: Prompt
defaultReflectSystemPrompt = Prompt $ T.unlines
  [ "Something just happened to a fictional character -- a scene they were present for, now finished."
  , "This isn't a plan anymore; it's what actually occurred. Process it as that character would: if"
  , "you noticed something about another present character that changes or corrects what your own"
  , "characters/*.md notes say about them, update that file. If something's worth a private thought or"
  , "a new suspicion -- prompted by what actually happened, not just what you expected going in --"
  , "record it with add_thought/add_suspicion. Both are entirely optional; only act if something"
  , "genuinely changed."
  , ""
  , "Once you're done (or if there's nothing to update), write this character's own journal entry:"
  , "their private, first-person account of the scene. Write only what this character actually"
  , "witnessed, did, or was told -- never anything only the reader or another character would know,"
  , "and never anything this character wasn't in a position to notice or wouldn't have understood."
  , "It's fine, even expected, for the entry to be incomplete, coloured by this character's own"
  , "concerns, or to misread someone else's motives -- that's what makes it a real journal entry"
  , "rather than a narrator's account. Write a few sentences to a short paragraph, in this character's"
  , "own voice. Your final reply, once you're done with any tool calls, is taken as the journal entry"
  , "verbatim -- output only that, nothing else."
  ]

-- | Same headroom reasoning as 'defaultCharacterConfig' -- this is the
--   config actually implicated in a thinking model returning an empty
--   journal entry: the final tool-loop turn has to fit both the model's
--   thinking tokens and the visible entry inside one budget.
defaultReflectConfig :: [ModelConfig AgentModel]
defaultReflectConfig = [MaxTokens 8000, Temperature 0.8]

defaultReflectInstructions :: Prompt
defaultReflectInstructions = "Update your notes or record a thought if anything actually changed, then write this character's journal entry for what just happened."
