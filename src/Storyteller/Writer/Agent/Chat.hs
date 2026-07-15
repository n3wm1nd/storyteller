{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The chat agent: discusses the story with the author rather than
-- continuing its prose. The file's own tick chain already stores the
-- conversation as alternating 'Storyteller.Writer.Agent.Prompt'/'Atom'
-- ticks (see 'historyFromFileTicks'), so no new tick kind is needed.
--
-- Unlike 'Storyteller.Writer.Agent.Continuation.proseAgent', this agent
-- gets no context handed to it up front — by default it sees nothing but
-- the conversation itself. Instead it can find and read files on the
-- current branch via tool calls ('glob'/'read_file'), the same
-- bind-a-real-effect-behind-a-tool pattern
-- 'Storyteller.Writer.Agent.ReplaceTool' uses, and the same read-only
-- agent-loop shape as @runix-code@'s @runixCodeAgentLoop@
-- (@../runix/apps/runix-code/lib/Agent.hs@): query, and if the model called
-- a tool, execute it and loop; otherwise return the text.
--
-- 'chatAgent' itself has no notion of "chat turn," "instruction," or
-- "reply" — it's just @context in, whatever this call contributed on top
-- of it out@, so continuing on top of it (elsewhere, later, with a
-- different caller entirely) is just calling it again with a longer
-- context. That's also what lets it self-recurse on tool calls directly
-- instead of needing a separate accumulator-shaped loop: every recursive
-- call has the exact same shape as the original one. Turning that into
-- "the user asked X, the agent said Y" (or a persisted atom, or a rendered
-- transcript) is the caller's job — see 'Server.Writer.File.chatConverse'.
--
-- All three tools are @Runix.Tools@'s own 'Tools.glob'/'Tools.readFile'/
-- 'Tools.sedPrint' — reused as-is, same behaviour as every other Runix
-- agent gets, not a reimplementation. @glob@'s pattern (e.g. @\"**\/*\"@ for
-- "everything") subsumes a plain listing, so there's no separate
-- list-files tool. @grep@/@diff@ aren't included: both need a real
-- filesystem path underneath ('Runix.Grep'/@diff@ shell out), which a
-- git-branch's virtual filesystem doesn't have — a pure, in-memory grep
-- would need its own effect the way 'Storyteller.Core.Git' gave 'Glob' one
-- (see below), not yet worth it without a concrete need.
--
-- Message order matters here for two reasons: correctness (a real
-- conversation has to replay in the order it happened) and prompt-cache
-- efficiency (providers that cache by prefix only get a hit if the prefix
-- is byte-identical to a previous call). Both fall out of the same
-- discipline: never rewrite or reorder anything already in the list, only
-- ever append.
--
--   * The system prompt is fully static — no per-call content (like the
--     old context dump) gets spliced into it, so it's byte-identical on
--     every call for every file, maximizing how much of the prefix a
--     provider can cache.
--   * 'historyFromFileTicks' replays prior turns oldest-first, and each
--     'Server.Writer.File.chatConverse' call only ever appends one more
--     exchange to the file's chain — so the history prefix one call sees is
--     always exactly the previous call's history plus one more turn, never
--     rewritten.
--   * Within one call, the tool loop below only ever appends (the model's
--     response, then tool results, then the next query) — never reorders or
--     drops earlier messages in the same turn.
--   * Deliberately NOT persisted: the tool-call exploration within a single
--     turn (any 'glob'/'read_file' round trips) isn't written back as
--     ticks — only the final reply is (see 'Server.Writer.File.chatConverse'
--     picking the 'AssistantText' pieces back out of what this returns).
--     So a later turn's history won't include an earlier turn's tool calls;
--     if the model needs that file again, it asks again. This keeps the
--     persisted history small and stable rather than growing every turn by
--     however much exploration happened, which matters for the same
--     cache-prefix-stability reason.
module Storyteller.Writer.Agent.Chat
  ( chatAgent
  , historyFromFileTicks
  ) where

import qualified Data.Text as T
import Polysemy
import Polysemy.Fail

import Runix.FileSystem (FileSystem, FileSystemRead)
import Runix.LLM (queryLLM)
import Runix.Logging (Logging, info)
import qualified Runix.Tools as Tools
import UniversalLLM (Message(..), ModelConfig(..), getToolCallName)
import UniversalLLM.Tools (LLMTool(..), llmToolToDefinition, executeToolCallFromList)

import Storyteller.Core.LLM.Role (LLMs, AgentModel)
import Storyteller.Core.Prompt (Prompt, PromptStorage, getConfigWithPrompt)
import Storage.Tick (FileTick(..))

-- | Continue a conversation: given everything the model should see so far
--   (prior history plus, at minimum, the new message the caller wants
--   answered), return whatever this call contributed on top of that —
--   any tool calls and their results, and the concluding reply. Re-fetches
--   the system prompt and rebuilds 'chatTools' on every recursive step
--   rather than hoisting them out of a separate non-recursive setup phase;
--   that's a deliberate simplicity-over-micro-optimisation call — an LLM
--   round trip dominates the cost of either by orders of magnitude, so
--   there's nothing worth the extra shape (and the loss of "the recursive
--   call looks exactly like the original one") to save it.
--   Logged turn by turn, same reasoning as
--   'Storyteller.Writer.Agent.Outline.splitOutlineAgent': a tool-exploring
--   conversation can run several real round trips (a 'glob' to find
--   something, then a 'read_file' on what it found, ...) before it settles
--   on a reply, so without a log line before each query, that whole
--   exploration looks identical to one long hang.
chatAgent
  :: forall branch r
  .  (LLMs r, Members '[PromptStorage, FileSystem branch, FileSystemRead branch, Fail, Logging] r)
  => [Message AgentModel]        -- ^ context to send: history plus this turn's new message(s) so far
  -> Sem r [Message AgentModel]  -- ^ everything this call added on top of the given context
chatAgent context = go (1 :: Int) context
  where
    go turnNo ctx = do
      configsWithPrompt <- getConfigWithPrompt "agent.chat" defaultChatSystemPrompt defaultChatConfig
      let tools = chatTools @branch @r
          configsWithTools = Tools (map llmToolToDefinition tools) : configsWithPrompt
      info $ "chatAgent: turn " <> T.pack (show turnNo) <> ": querying model..."
      response <- queryLLM configsWithTools ctx
      case [tc | AssistantTool tc <- response] of
        [] -> return response
        calls -> do
          mapM_ (\tc -> info ("chatAgent: turn " <> T.pack (show turnNo) <> ": calling " <> getToolCallName tc)) calls
          results <- mapM (executeToolCallFromList tools) calls
          let added = response ++ map ToolResultMsg results
          rest <- go (turnNo + 1) (ctx ++ added)
          return (added ++ rest)

-- | Fallback for @agent.chat@ (the namespace root -- see
--   'Storyteller.Core.Prompt' on why the root is implicitly the system
--   prompt/config, not @agent.chat.system@), used until an override is committed
--   to the 'Storyteller.Core.Runtime.Prompts' branch. Deliberately static —
--   see the module header on why nothing gets templated into this per call.
defaultChatSystemPrompt :: Prompt
defaultChatSystemPrompt =
  "You are the author's discussion partner for this story. Talk through \
  \ideas, answer questions, and brainstorm — do not write story prose \
  \unless explicitly asked to. You start out seeing only this conversation; \
  \use glob (e.g. \"**/*\" for everything), read_file, and sed_print (for a \
  \line range out of a long file) to look at the rest of the project \
  \whenever you need to, rather than assuming or guessing at their \
  \contents."

-- | Compiled-in sampling default for @agent.chat@ -- see @$key.llmsettings.
--   yaml@ overrides via 'Storyteller.Core.Prompt.getConfig'. A conversational
--   reply, not a whole chapter or a single-atom edit -- middling budget,
--   middling temperature (natural, but not creative-writing-varied).
defaultChatConfig :: [ModelConfig AgentModel]
defaultChatConfig = [MaxTokens 2048, Temperature 0.8]

-- | The model's window into the branch: find paths by pattern, read one
--   back by exact path, or pull just a line range out of a long one.
--   Deliberately read-only — no write access, no grep — this agent
--   discusses, it doesn't edit. All three are 'Runix.Tools' functions
--   already carrying their own name/description via their result types'
--   'ToolFunction' instances, so no 'mkTool' wrapper needed.
chatTools
  :: forall branch r
  .  Members '[FileSystem branch, FileSystemRead branch, Fail] r
  => [LLMTool (Sem r)]
chatTools =
  [ LLMTool (Tools.glob @branch)
  , LLMTool (Tools.readFile @branch)
  , LLMTool (Tools.sedPrint @branch)
  ]

-- | A file's own tick chain, oldest-first, already interleaves the user's
--   messages ('Storyteller.Writer.Agent.Prompt' ticks) with the agent's
--   replies ('Storyteller.Core.Atom.Atom' ticks) — this is exactly a
--   conversation transcript, so building one is just filtering and
--   relabelling, not new storage. Any other tick kind on the file (notes,
--   presence) is conversational noise here and dropped -- and so is any
--   tick the user has explicitly hidden (@("hide", "true")@ in 'ftFields',
--   set\/cleared via 'Server.Core.File.hideFileAtoms'\/'unhideFileAtoms'):
--   this is the one place a hidden tick actually stops reaching a model,
--   same tag the UI already lets a user toggle per atom but that, until
--   now, only ever affected how the atom was *displayed*, never what a
--   later call here actually sent.
historyFromFileTicks :: [FileTick] -> [Message m]
historyFromFileTicks = concatMap toMessage . filter (not . isHidden)
  where
    isHidden ft = lookup "hide" (ftFields ft) == Just "true"

    toMessage ft = case ftKind ft of
      "prompt" -> [UserText (ftMessage ft)]
      "atom"   -> [AssistantText (maybe (ftMessage ft) id (ftContent ft))]
      _        -> []
