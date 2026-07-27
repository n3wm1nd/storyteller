{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | User-defined writers: an agent whose /entire/ behaviour is two files a
-- project commits, with no compiled-in counterpart at all.
--
-- Every other agent in this directory is a Haskell function that decides
-- what its own context is (see 'Storyteller.Writer.Agent.Write.writeAgent':
-- chapters, style, who's present, journal excerpts, the reconstructed
-- conversation -- all agent-owned, deliberately not caller-suppliable).
-- That's the right shape for the agents we ship, because we know what a
-- writer needs better than a caller does. It's exactly the wrong shape for
-- an agent someone invents for their own project: there is no compiled-in
-- knowledge to defend, so this module has none. It resolves the program,
-- renders it, appends the instruction, and calls the model:
--
--   1. @context.custom.\<slug\>@ (a @.dsl@ on the 'Contexts' branch),
--      resolved at arity 1 with this file's path -- the same shape
--      'Storyteller.Context.DSL.Library.contextWriter' has, so the
--      seeded default for a new agent (@path: context.writer path@) is a
--      working agent from the first save, and narrowing it from there is
--      ordinary DSL editing.
--   2. Every message that program produced, in its own order.
--   3. This call's pinned content -- the user's own explicit atom
--      selection, @\@mention@s, and any per-call pinned programs the
--      context pull-up added. Not part of the agent's definition at all:
--      it's the same per-call data 'Storyteller.Writer.Agent.Write.writeAgent'
--      takes as a 'PinnedContext' parameter, resolved identically by the
--      same caller, and it rides along here for the same reason -- a
--      custom agent reached from the composer has to honour the composer's
--      own context controls, or those controls would be silently inert in
--      exactly the mode a user is most likely to be experimenting in.
--   4. @UserText instruction@, last -- byte-identical to what a @"prompt"@
--      tick replays as, the same discipline
--      'Storyteller.Writer.Agent.Write' documents.
--
-- __Nothing else is implicit__, and that's the design, not an omission.
-- No character context, no tick history, no style splice: a custom agent
-- sees precisely what its program says and nothing more, which is what
-- makes "fully defined by the two committed files" a true statement rather
-- than an approximate one. The pieces the built-in writer gathers for
-- itself are all reachable /by name/ from the DSL anyway
-- (@context.writer@, @context.character@, @context.chapters@, ...), so
-- opting into any of them is one line of the user's own program -- an
-- inclusion they can see, rather than a behaviour they have to infer.
--
-- The system prompt/sampling side is ordinary 'PromptStorage' under the
-- matching @agent.custom.\<slug\>@ key, so the Agents tab's existing
-- prompt/config editors work on one of these with no special casing: a
-- custom agent is an @AgentDef@ whose keys happen to be discovered at
-- runtime rather than compiled in.
--
-- Always the 'Storyteller.Core.LLM.Role.ProseModel' role -- these write
-- into a prose file, and their output is split and appended exactly like
-- the writer's (see 'Server.Writer.File.customWriter'). A custom agent
-- that wants to reason rather than write is a real want, but it's a
-- different role and therefore a different call path (see
-- @project_toolprose_role_plan@'s two-agent-function pattern); it isn't
-- expressible by editing branch files, so it isn't offered here.
module Storyteller.Writer.Agent.Custom
  ( customAgent
  , customContextName
  , customPromptKey
  , buildCustomMessages
  , defaultCustomSystemPrompt
  , defaultCustomConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Polysemy
import Polysemy.Fail (Fail)
import Runix.LLM (queryLLM)
import Runix.Logging (Logging, info)
import UniversalLLM (Message(..), ModelConfig(..))

import Storyteller.Context.DSL.Render (dslMessageToLLM)
import Storyteller.Context.DSL.Value (messageText)
import Storyteller.Core.LLM.Role (LLMs, ProseModel)
import Storyteller.Core.Prompt (Prompt(..), PromptKey(..), PromptStorage, getConfigWithPrompt)
import Storyteller.Context.DSL.AST (Name)
import Storyteller.Writer.Agent (Instruction(..), Prose(..))
import Storyteller.Writer.Agent.Context (PinnedContext(..), ProgramContext(..))

-- | The Context DSL slot a custom agent's own context program lives at --
--   @context/custom/\<slug\>.dsl@ on the 'Contexts' branch, by the ordinary
--   dots-as-slashes rule ('Storyteller.Core.Context'). The @custom.@
--   segment keeps a project's own agent names clear of every compiled-in
--   @context.*@ slot, so inventing an agent can never shadow one.
customContextName :: Text -> Name
customContextName slug = "context.custom." <> slug

-- | The 'PromptStorage' key holding the same agent's system prompt
--   (@agent/custom/\<slug\>.md@) and sampling config
--   (@agent/custom/\<slug\>.llmsettings.yaml@) on the 'Prompts' branch --
--   deliberately the mirror image of 'customContextName', so one slug
--   names both halves of an agent and the frontend can enumerate agents
--   from either branch's file list alone.
customPromptKey :: Text -> PromptKey
customPromptKey slug = PromptKey ("agent.custom." <> slug)

-- | Run the custom agent named @slug@.
--
--   Both contexts arrive already resolved, so this needs no storage
--   capability: the caller looks the program up by
--   'customContextName' (and fails loudly when the slug names no program
--   -- an agent whose whole definition is missing has nothing to fall back
--   to, unlike an overridable slot) and resolves this call's pinned
--   content. See 'Server.Writer.File.customWriter'. What's left here is
--   this agent's own subject: prompt key, message ordering, defaults.
customAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail, Logging] r)
  => Text                        -- ^ agent slug, e.g. @"critic"@ -- prompt-key only; the program itself already arrived as 'ProgramContext'
  -> ProgramContext              -- ^ this agent's own program, resolved by the caller
  -> PinnedContext               -- ^ this call's pinned content, also caller-resolved -- per-call user data, not part of the agent's definition
  -> Instruction
  -> Sem r Prose
customAgent slug (ProgramContext context) (PinnedContext pinned) instruction = do
  configsWithPrompt <- getConfigWithPrompt (customPromptKey slug) defaultCustomSystemPrompt defaultCustomConfig
  info ("customAgent (" <> slug <> "): querying model...")
  response <- queryLLM configsWithPrompt
    (buildCustomMessages (map dslMessageToLLM context)
                         (T.intercalate "\n\n" (map messageText pinned))
                         instruction)
  return $ Prose $ mconcat [ t | AssistantText t <- response ]

-- | The whole of this agent's message-order policy, with no effect
--   attached: the program's own stream, then this call's pinned content
--   (dropped entirely when empty, rather than sent as a blank message),
--   then the instruction, last. Split out for the same reason
--   'Storyteller.Writer.Agent.Write.buildChapterMessages' is -- the
--   ordering claim in this module's Haddock should be assertable without a
--   model call, even when (especially when) the ordering is this small.
--
--   Pinned content sits /after/ the program's own stream, not spliced into
--   it: everything the program produced is this agent's stable, cacheable
--   prefix across a run of turns, and pinned content is by definition the
--   part the user just changed. Splicing it mid-history the way
--   'Storyteller.Writer.Agent.Write.buildChapterMessages' does would be
--   guessing at where a /user-authored/ program's turn boundaries are; a
--   program that wants that control has @embedshallow@ and can place its
--   own conversation read wherever it likes.
buildCustomMessages :: [Message m] -> Text -> Instruction -> [Message m]
buildCustomMessages context pinned (Instruction instr) =
  context ++ [ UserText pinned | not (T.null pinned) ] ++ [UserText instr]

-- | Fallback system prompt, used until a project commits
--   @agent/custom/\<slug\>.md@. Deliberately near-contentless: a custom
--   agent's persona is the entire point of the prompt file, so shipping
--   an opinionated default here would be putting words in a user's mouth.
--   It says only the one thing that's true of every agent reached through
--   this path (its output is appended to the open file as prose), so a
--   half-configured agent still produces something usable rather than
--   commentary about the request.
defaultCustomSystemPrompt :: Prompt
defaultCustomSystemPrompt =
  "You are a writing assistant. Output only the text to append to the \
  \document, with no preamble, commentary, or restatement of the request."

-- | Compiled-in sampling default, matching @agent.writer@'s
--   ('Storyteller.Writer.Agent.Continuation.defaultWriterConfig') -- a
--   custom agent lands in the same file, through the same splitter, so
--   starting it at different headroom or temperature than the writer would
--   be an arbitrary difference to explain. Overridable per agent via
--   @agent/custom/\<slug\>.llmsettings.yaml@.
defaultCustomConfig :: [ModelConfig ProseModel]
defaultCustomConfig = [MaxTokens 3000, Temperature 0.9]
