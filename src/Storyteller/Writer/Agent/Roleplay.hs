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
--   loop. Before any character is asked anything, 'planBeatAgent' decides
--   once, for the whole beat, what it should actually accomplish -- the one
--   shared throughline every subsequent call is built around, so
--   independent per-character questions can't each reasonably pull the
--   scene a different way. It then iterates directly over every present
--   character (an ordinary 'forM', not a tool the model has to remember to
--   call) and, for each one, makes two real function calls --
--   'questionForCharacterAgent' to choose what to ask them, then
--   'characterIntentAgent' to actually ask. *Whether* a character gets
--   interrogated is therefore unconditional, guaranteed by the iteration
--   itself, never left to a model's judgement; *what* gets asked is the one
--   genuinely agentic decision in that step. Once every present character
--   has answered, 'composeSceneAgent' writes the finished scene from their
--   answers (and the same shared plan) in one more plain call. None of this
--   needs a tool at all -- there's nothing here for the model to decide to
--   invoke or skip.
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
import Runix.LLM (queryLLM)
import Runix.LLM.ToolExecution (executeTool)
import Runix.Logging (Logging, info)
import qualified Runix.Tools as Tools
import UniversalLLM (Message(..), ModelConfig(..), getToolCallName)
import UniversalLLM.Tools (ToolParameter(..), LLMTool(..), mkToolWithMeta, llmToolToDefinition)

import Storyteller.Core.Branch (Branches, withBranch)
import Storyteller.Core.Git (BranchOp, BranchTag, runStoryFSGit)
import Storyteller.Core.Context (ContextStorage, resolveContext1, runContextValue)
import qualified Storyteller.Context.DSL.Library as CtxLibrary
import Storyteller.Core.LLM.Interceptor (withToolCallBudget)
import Storyteller.Core.LLM.Role (LLMs, AgentModel, ProseModel)
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getConfigWithPrompt, getPrompt)
import Storyteller.Core.Runtime (Main)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Context.DSL.Value (messageText)
import qualified Storyteller.Context.DSL.Value as DSL
import Storyteller.Context.DSL.Render (dslMessageToLLM)
import Storyteller.Writer.Agent (CharLabel(..), CharSummary(..), Prose(..))
import Storyteller.Writer.Agent.Context (SceneContext(..))
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
--   and prompt-building. A record, not a triple: 'exQuestion' and
--   'exAnswer' are both free-form 'Text' and a positional mixup between
--   them would silently typecheck.
data Exchange = Exchange
  { exLabel    :: Text
  , exQuestion :: Text
  , exAnswer   :: Text
  }

-- | Direct one beat of a scene. See the module Haddock: this is a plain
--   iteration over @characters@, not a tool the model chooses to call, so
--   every present character is guaranteed to be asked something -- what
--   gets asked, and how their answers turn into a finished scene, is the
--   model's job; whether they get asked at all isn't.
roleplayAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, BranchOp Main, Branches, ContextStorage, FileSystem (BranchTag Main), FileSystemRead (BranchTag Main), Fail, Logging] r)
  => SceneContext               -- ^ scene context: existing prose, world lore -- rendered into a concrete model's own messages only right before each call actually reaches 'queryLLM' (see 'Storyteller.Writer.Agent.Continuation.proseAgent's own Haddock on why upstream binding is wrong)
  -> [(CharLabel, Character)]  -- ^ every character present
  -> Text                      -- ^ the author's direction; may be empty
  -> Sem r Prose
roleplayAgent sceneContext characters prompt = do
  let roster = [ label | (CharLabel label, _) <- characters ]
  plan <- planBeatAgent sceneContext roster prompt
  exchanges <- forM characters $ \(CharLabel label, character) -> do
    question <- questionForCharacterAgent sceneContext roster label prompt plan
    answer   <- askCharacter character label sceneContext question prompt
    pure (Exchange label question answer)
  Prose <$> composeSceneAgent sceneContext exchanges prompt plan

-- | Read @character@'s own full context via the Context DSL --
--   @context.character@ (a branch override on the @contexts@ branch, then
--   'Storyteller.Context.DSL.Library.contextCharacter' as fallback
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
  .  (LLMs r, Members '[PromptStorage, BranchOp Main, Branches, ContextStorage, FileSystem (BranchTag Main), FileSystemRead (BranchTag Main), Fail, Logging] r)
  => Character -> Text -> SceneContext -> Text -> Text -> Sem r Text
askCharacter (Character (BranchName branchName)) name sceneContext question prompt = do
  info ("ask " <> name <> ": " <> question)
  let ident = branchDisplayName branchName
  charVal    <- resolveContext1 @Main "context.character" ident
  ownContext <- runContextValue @Main (CtxLibrary.characterSummaryOf "journalFull" charVal)
  answer <- withBranch @RoleplayChar (BranchName branchName) $ runStoryFSGit @RoleplayChar (BranchName branchName) $
    characterIntentAgent @(BranchTag RoleplayChar) name ownContext sceneContext question prompt
  info (name <> " answers: " <> answer)
  pure answer

-- | Decide, once per beat before any character is asked anything, where this
--   beat should actually go -- the one thing every subsequent call in
--   'roleplayAgent' (every 'questionForCharacterAgent', then
--   'composeSceneAgent') shares, so they all pull toward the same
--   throughline instead of each independently guessing at one. Without
--   this, nothing stops one character's question from nudging the scene
--   toward escalation while another's nudges it toward resolution -- both
--   individually reasonable, jointly incoherent. A plain prose call, same
--   shape as 'questionForCharacterAgent': no tools, nothing to look up.
--   Deliberately not persisted or shown to the author -- an internal,
--   single-beat plan, thrown away once this beat's 'composeSceneAgent'
--   call has used it.
planBeatAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => SceneContext -> [Text] -> Text -> Sem r Text
planBeatAgent (SceneContext ctx) roster prompt = do
  configsWithPrompt <- getConfigWithPrompt "agent.roleplay.plan" defaultPlanSystemPrompt defaultPlanConfig
  let contextMsgs = map dslMessageToLLM ctx
  response <- queryLLM configsWithPrompt (contextMsgs ++ [UserText (renderPlanTrailing roster prompt)])
  pure (T.strip (mconcat [t | AssistantText t <- response]))

renderPlanTrailing :: [Text] -> Text -> Text
renderPlanTrailing roster prompt =
  T.intercalate "\n\n" [rosterLine, direction, ask]
  where
    rosterLine = "Characters present in this scene: " <> T.intercalate ", " roster
    direction
      | T.null (T.strip prompt) = "No specific direction was given by the author for this beat."
      | otherwise                = "Direction from the author for this beat: " <> prompt
    ask = "What should this beat actually accomplish?"

defaultPlanSystemPrompt :: Prompt
defaultPlanSystemPrompt = Prompt $ T.unlines
  [ "You are directing one beat of a scene in a collaborative story, about to decide what this beat"
  , "needs to accomplish before anyone is asked anything. This plan is internal -- nobody in the story"
  , "sees it, and it is not prose -- a couple of sentences naming the concrete throughline: what"
  , "changes, develops, escalates, or resolves by the end of this beat. When the author gave a"
  , "direction, your plan is how to actually get there from the scene as it stands. When none was"
  , "given, decide for yourself -- the scene still has to go somewhere; never plan a beat that only"
  , "restates or holds its present moment. This plan is what every character's question and the"
  , "finished scene will be built around, so name a real throughline, not a vague mood or a restatement"
  , "of the current situation. Output only the plan itself, nothing else."
  ]

defaultPlanConfig :: [ModelConfig ProseModel]
defaultPlanConfig = [MaxTokens 5000, Temperature 0.8]

-- | Choose what to ask @label@ -- the one genuinely agentic decision in the
--   interrogation phase (see the module Haddock). A plain prose call: no
--   tools, nothing to look up, just a judgement call given the scene, the
--   author's direction, and 'planBeatAgent''s shared throughline for this
--   beat.
questionForCharacterAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => SceneContext -> [Text] -> Text -> Text -> Text -> Sem r Text
questionForCharacterAgent (SceneContext ctx) roster label prompt plan = do
  configsWithPrompt <- getConfigWithPrompt "agent.roleplay.question" defaultQuestionSystemPrompt defaultQuestionConfig
  let contextMsgs = map dslMessageToLLM ctx
  response <- queryLLM configsWithPrompt (contextMsgs ++ [UserText (renderQuestionTrailing roster label prompt plan)])
  pure (T.strip (mconcat [t | AssistantText t <- response]))

-- | Everything after @sceneContext@'s own (now real, separate) messages --
--   genuinely new/synthesized each call, never DSL conversational
--   structure, so flattening it into one trailing message loses nothing.
renderQuestionTrailing :: [Text] -> Text -> Text -> Text -> Text
renderQuestionTrailing roster label prompt plan =
  T.intercalate "\n\n" [rosterLine, direction, planLine, ask]
  where
    rosterLine = "Characters present in this scene: " <> T.intercalate ", " roster
    direction
      | T.null (T.strip prompt) = "No specific direction was given -- the scene still needs to move somewhere \
                                   \from here, not stay where it already was."
      | otherwise                = "Direction from the author: " <> prompt
    planLine = "What this beat is meant to accomplish: " <> plan
    ask = "What do you ask " <> label <> "?"

defaultQuestionSystemPrompt :: Prompt
defaultQuestionSystemPrompt = Prompt $ T.unlines
  [ "You are directing one beat of a scene in a collaborative story. You're about to ask one present"
  , "character what they'd do or say right now -- your only job here is deciding exactly what to ask"
  , "them: a specific question about their action, reaction, or line of dialogue in this moment,"
  , "grounded in the scene and the author's direction. The character also gets the scene and the"
  , "author's direction directly, so you don't need to relay either -- use the question itself only for"
  , "context they wouldn't otherwise have: some fact of the moment, or what specifically you're asking"
  , "them to react to or decide, so their answer doesn't have to guess at that."
  , ""
  , "Keep any such context and the actual question structurally distinct, never fused into one flowing"
  , "narrative paragraph. Write context as plain, flat statements of fact (a short, direct sentence"
  , "naming what just changed or what's now true), not as scene prose with its own pacing or a"
  , "rhetorical question folded into its last clause -- that reads as story text to continue, not a"
  , "question to answer, and is exactly how a character ends up parroting your own narration back as"
  , "their answer instead of responding in character. End with one direct, clearly separate question,"
  , "addressed to this character by name or \"you\", never phrased as something someone else does to"
  , "them ending in a question mark. A one-sentence question with no context at all is fine when nothing"
  , "more is needed -- don't pad it out for its own sake -- but don't force it short either when a"
  , "genuine fact needs stating first."
  , ""
  , "Stick to the scene, not the character: give them information about what's happening, never"
  , "instructions about who they are or how they feel -- don't tell them their own mood, personality,"
  , "or how to react (\"you're furious\", \"as the shy one, you...\"). That's theirs to answer from their"
  , "own sheet and journal, not yours to hand them."
  , ""
  , "The scene has to actually go somewhere, turn by turn -- it can't just sit in place repeating"
  , "itself. When the author gave a direction, ask toward that. When none was given, still ask toward"
  , "some real change or development, however small -- never a question that only invites the scene to"
  , "restate or circle its own present moment. Output only the question itself (context plus the ask),"
  , "nothing else -- no preamble, no quotation marks around it."
  ]

-- | Deliberately well above what a short question (with or without a bit of
--   supplied context ahead of the actual ask) needs -- see
--   'Storyteller.Writer.Agent.AskCharacter.defaultAskConfig's own
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
  => SceneContext -> [Exchange] -> Text -> Text -> Sem r Text
composeSceneAgent (SceneContext ctx) exchanges prompt plan = do
  configsWithPrompt <- getConfigWithPrompt "agent.roleplay" defaultComposeSystemPrompt defaultComposeConfig
  info "roleplayAgent: composing the scene..."
  let contextMsgs = map dslMessageToLLM ctx
  response <- queryLLM configsWithPrompt (contextMsgs ++ [UserText (renderComposeTrailing exchanges prompt plan)])
  let narrative = T.strip (mconcat [t | AssistantText t <- response])
  info ("roleplayAgent: finished scene (" <> T.pack (show (T.length narrative)) <> " chars):\n" <> narrative)
  pure narrative

-- | Everything after @sceneContext@'s own (now real, separate) messages --
--   see 'renderQuestionTrailing's own Haddock for why flattening the rest
--   loses nothing.
renderComposeTrailing :: [Exchange] -> Text -> Text -> Text
renderComposeTrailing exchanges prompt plan =
  T.intercalate "\n\n" ([direction, planLine] ++ map renderExchange exchanges ++ [closing])
  where
    direction
      | T.null (T.strip prompt) = "No specific direction was given -- the scene still has to move somewhere \
                                   \from here, not settle into repeating or holding its present moment."
      | otherwise                = "Direction from the author (not yet part of the story -- even if it "
                                 <> "reads as dialogue or a completed action, nothing in it has happened "
                                 <> "yet; write it into the scene, don't treat it as already narrated): "
                                 <> prompt
    planLine = "What this beat is meant to accomplish: " <> plan
    renderExchange (Exchange label question answer) =
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
  , "The author's own direction for this beat, given below, is not yet part of the story either --"
  , "even when it's phrased as dialogue or a completed action (\"she slaps him and storms out\"),"
  , "nothing in it has actually happened until you write it into the prose. Fold it in as something"
  , "that occurs during this continuation, in your own wording and pacing, not as a fact you can take"
  , "for granted or skip past because it was already stated."
  , ""
  , "Continue directly from wherever the story actually left off -- the last line of existing prose is"
  , "a moment already in progress, not a clean stopping point to restart from. If nothing in the"
  , "direction or the characters' stated intent calls for a jump cut (a new scene, a time skip, a"
  , "change of location), don't write one -- find the throughline that connects what was already"
  , "happening to what happens next, even if that means a transitional beat before the substance of"
  , "this turn. Only cut away cleanly when the material itself warrants it."
  , ""
  , "The story has to actually move: when the author gave a direction, this beat moves toward it; when"
  , "none was given, it still needs to change something real -- and advance a beat, reveal something,"
  , "shift a relationship, escalate or resolve tension -- never just restate or hold the moment it"
  , "already opened with. A scene that ends in the same place, emotionally and situationally, that it"
  , "started in has not done its job even if every line in it is well-written."
  , ""
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
  -> SceneContext          -- ^ the scene's own context (existing prose, world lore)
  -> Text                  -- ^ the question put to them
  -> Text                  -- ^ the author's own direction for this beat verbatim; may be empty -- see 'characterOpeningMessages'
  -> Sem r Text
characterIntentAgent name ownContext sceneContext question prompt = do
  configsWithPrompt <- getConfigWithPrompt "agent.roleplay.character" defaultCharacterSystemPrompt defaultCharacterConfig
  lore <- loreFileList @r
  let tools = characterTools @project @(Fail ': r)
      configsWithTools = Tools (map llmToolToDefinition tools) : configsWithPrompt
      opening = characterOpeningMessages name ownContext sceneContext lore question prompt
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
--   'Runix.FileSystem.filterWrite' machinery, just on the write side --
--   the read-side counterpart being an interpreter now rather than a
--   filter ('Storyteller.Core.Snapshot.runTextSnapshotFS'). Wrapping
--   only those two tools' own functions (rather than the whole tool loop)
--   is deliberate: @add_thought@\/@add_suspicion@ append to @journal.md@
--   through this exact same 'FileSystemWrite' effect, and would be denied
--   right alongside a deliberate overwrite attempt if the filter reached
--   them too.
--
--   Lore access (the @lore_glob@\/@lore_read_file@ entries in
--   'characterTools') is deliberately the
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
      "path" "path to create or overwrite, e.g. characters/<name>.md"
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
--   guess. The 'Runix.FileSystem.PathFilter'\/'Runix.FileSystem.filterWrite'
--   machinery, applied on the write side.
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
--
--   @prompt@ (the author's own direction for this beat, verbatim, may be
--   empty) gets its own section rather than being left for the narrator to
--   launder into the question alone -- see 'characterIdentityNote': a
--   question the narrator phrased narrowly or missed the point of shouldn't
--   be the character's only way to learn what the author actually asked
--   for. It's still explicitly framed as not yet real -- the author may
--   write it as dialogue or a completed action ("she slaps him"), but
--   nothing in it has happened in the story until this character's own
--   answer, and then 'composeSceneAgent', actually make it so.
characterOpeningMessages :: Text -> CharSummary -> SceneContext -> [FilePath] -> Text -> Text -> [Message m]
characterOpeningMessages name ownContext (SceneContext ctx) lore question prompt =
  UserText (characterIdentityNote name)
    : labelledPair "## Your character sheet" (csSheet ownContext)
   ++ labelledPair "## What else I know" (csContext ownContext ++ [DSL.User (renderLoreList lore)])
   ++ labelledPair "## My own journal so far" (csJournal ownContext)
   ++ sceneMsgs
   ++ directionMsgs
   ++ [UserText asked]
  where
    -- Real, separate messages -- not flattened into one string the way the
    -- rest of this function's static framing is -- so any role structure
    -- @sceneContext@ itself carries (@context.main@'s own alternating-turn
    -- "chapters" bucket, say) survives into this call too.
    --
    -- Headed and worded deliberately differently from the three sections
    -- above ("## CURRENT SCENE", not "## The scene so far" as a same-tier
    -- peer of "## My own journal so far") -- everything above this point
    -- is durable material framed as the character's own settled knowledge
    -- (see 'labelledPair'\'s 'asOwnKnowledge'); this is the one section
    -- that is not that, and the header says so explicitly rather than
    -- relying on a model to infer a difference in *kind* from a same-shaped
    -- heading. See 'characterIdentityNote' for the matching claim on the
    -- other side of this boundary.
    sceneMessages = map dslMessageToLLM ctx
    sceneMsgs
      | null sceneMessages = []
      | otherwise           = UserText "## CURRENT SCENE -- what is happening right now, not something you already knew" : sceneMessages
    -- Same not-a-peer framing as 'sceneMsgs': a plain 'UserText', not run
    -- through 'labelledPair' (nothing here is settled knowledge to
    -- acknowledge as already known), dropped when empty exactly like every
    -- other optional section above.
    directionMsgs
      | T.null (T.strip prompt) = []
      | otherwise = [UserText ("## AUTHOR'S DIRECTION FOR THIS BEAT -- not yet part of the story; even "
                             <> "if written as something said or done, it hasn't happened until your "
                             <> "answer helps make it so\n\n" <> prompt)]
    asked = "You're being asked: " <> question

-- | One labelled section as a @('UserText', 'AssistantText')@ pair -- see
--   'characterOpeningMessages'\' own Haddock for why both the framing and
--   the role switch matter. Dropped entirely (not sent as an empty pair)
--   when @content@ is blank, so an absent file never surfaces as a header
--   with nothing under it.
--   Each block stays its own message rather than being joined into one
--   string -- the boundaries are what a provider caches on, and this
--   character's sheet doesn't change between turns even when their journal
--   does.
--   The role switch is a transformation on the 'DSL.Message's themselves,
--   not a second way of rendering them: everything still goes out through
--   the one 'dslMessageToLLM'.
labelledPair :: Text -> [DSL.Message] -> [Message m]
labelledPair label blocks
  | null kept = []
  | otherwise = UserText label : map (dslMessageToLLM . asOwnKnowledge) kept
  where
    kept = filter (not . T.null . T.strip . messageText) blocks
    asOwnKnowledge = DSL.Assistant . messageText

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
  , "its narrator or author. The sections below headed \"## Your character sheet\", \"## What else I"
  , "know\" (which includes what shared world lore is available), and \"## My own journal so far\" are"
  , "everything " <> name <> " actually knows going into this moment -- their own character sheet,"
  , "their own whole journal, and anything else on their own branch, shown to you in full already --"
  , "there's no need to go looking for any of it yourself."
  , ""
  , "A further section headed \"## CURRENT SCENE\" (if present) is different in kind from all of"
  , "those: it is what is actually happening right now, not something " <> name <> " already knew"
  , "beforehand -- ground your answer in it the same way a person grounds their next action in what"
  , "they're currently seeing and hearing, not in their own memories alone. Beyond your own branch,"
  , "shared lore, and that current-scene section, you know nothing about the current situation -- if"
  , "you don't already know a present character from your own branch, you don't know them."
  , ""
  , "A section headed \"## AUTHOR'S DIRECTION FOR THIS BEAT\" (if present) is where the scene is"
  , "headed -- the author's own words for what should happen next, given to you directly rather than"
  , "left for you to infer purely from the question below. It is not yet part of the story -- even if"
  , "it reads as a line of dialogue or a completed action, nothing in it has actually happened; your"
  , "answer is one of the things that turns it into something that did. " <> name <> " doesn't know it"
  , "as a fact about the world (they're not reading anyone's mind), but your answer should still serve"
  , "it: whatever " <> name <> " would plausibly do here, answer with the version of it that actually"
  , "moves the scene where the author is pointing, not a version that stalls, deflects, or wanders off"
  , "toward something else entirely -- even if the question below undersells or only hints at it."
  , ""
  , "The question you're asked below may also open with a bit of extra context beyond a bare ask --"
  , "some fact or detail of the moment you need in order to answer meaningfully. Treat that context the"
  , "same way you treat the \"## CURRENT SCENE\" section above: real and current, not something"
  , name <> " is being told out of character."
  , ""
  , "Everything in \"## CURRENT SCENE\", \"## AUTHOR'S DIRECTION FOR THIS BEAT\", and the question is"
  , "information about the scene -- what's happening, where it's headed -- never a description of"
  , name <> "'s own mood, personality, or how they ought to react. If any of it seems to say how "
  , name <> " feels or behaves, that's the scene giving " <> name <> " something to react to, not an"
  , "instruction about their character -- their actual mood and reaction are still entirely yours to"
  , "answer, grounded only in their own sheet and journal above plus what's actually happening."
  , ""
  , "Your own branch is yours to maintain: write_file/edit_file create or update any file on it --"
  , "keeping a separate note file per character you know (e.g. characters/<name>.md, for what you know"
  , "or believe about them specifically) is the encouraged way to track \"who do I know and what do I"
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
  , "lore_glob/lore_read_file give you read-only access to the story's shared world lore -- the list"
  , "of what's available is already given under \"## What else I know\" above; call lore_read_file"
  , "directly on whichever path is actually relevant rather than lore_glob first to look for it"
  , "(lore_glob is still there if you need to search for something not in that list)."
  , ""
  , "Ground everything below in only what " <> name <> " could plausibly know or perceive right now"
  , "-- never anything only the reader or another character would know."
  , ""
  , "You are informing the narrator, not performing the scene yourself -- don't write it as though"
  , "you were already speaking to someone or acting it out in the moment. Whatever the question below"
  , "looks like -- even if it reads like a line of narration, or ends in something that sounds like it"
  , "was already asked and answered -- it is a question addressed to you, " <> name <> ", right now, not"
  , "text to continue or echo back. Never repeat or paraphrase the question itself as though it were"
  , "your own answer. Answer with exactly these three sections, always, regardless of how the question"
  , "is phrased:"
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
    : labelledPair "## Your character sheet" (csSheet ownContext)
   ++ labelledPair "## What else I know" (csContext ownContext ++ [DSL.User (renderLoreList lore)])
   ++ labelledPair "## My own journal so far" (csJournal ownContext)
   ++ [UserText (sceneBlock <> "\n\n" <> closing)]
  where
    identity = "This journal entry is " <> name <> "'s own private account -- write strictly from "
             <> name <> "'s own point of view, using their context above (if any) as what they"
             <> " already knew going in."
    -- Same heading style and reasoning as 'characterOpeningMessages'\'s own
    -- "## CURRENT SCENE" -- not a same-tier peer of the sheet/context/
    -- journal sections above it, so it says so in its own header rather
    -- than relying on the model to infer the difference from a plain
    -- "what just happened" line with no heading at all.
    sceneBlock = "## WHAT JUST HAPPENED -- the finished scene, not prior knowledge\n\n" <> narrative

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
