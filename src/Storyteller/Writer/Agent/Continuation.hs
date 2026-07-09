{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Prose generation agents.
--
-- 'proseAgent' is the pure LLM core: given assembled context, produce new
-- text. No filesystem access — all context is passed in explicitly, making
-- it easy to extend with new effects (style guides, persona, etc.) in isolation.
--
-- 'proseAgent' doesn't need to read a file — it needs to know that file's
-- content. 'gatherFileContext' is the machinery that answers that: it reads
-- the target file and every other branch file, and hands back plain data.
-- Callers compose the two themselves (@gatherFileContext >=> ...@), the same
-- way appending an agent's output is the caller's job on the write side —
-- keeps the read side and the write side symmetric.
module Storyteller.Writer.Agent.Continuation
  ( proseAgent
  , gatherFileContext
  ) where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Polysemy
import Polysemy.Fail
import Runix.FileSystem (FileSystem, FileSystemRead, fileExists, listAllFiles, readFile)
import Runix.LLM (queryLLM)
import UniversalLLM (Message(..), ModelConfig(..))

import Storyteller.Core.LLM.Role (LLMs, ProseModel)
import Storyteller.Writer.Agent (Instruction(..), Prose(..), CharContextBlock(..), ContextBlock(..), ExistingContent(..), WordCount(..))
import Storyteller.Writer.Agent.ContextFilter (ContextLayout, applyContextLayout)
import Storyteller.Core.Prompt (Prompt(..), PromptStorage, getPrompt, getConfigWithPrompt)

import Prelude hiding (readFile)

-- | Ask the LLM to produce new prose given fully assembled context.
--   No filesystem access — all inputs are explicit.
--   This is the composition point for cross-cutting concerns: style guides,
--   persona, tone constraints, etc. can be added here as new effects.
--
--   Always the 'ProseModel' role -- see 'Storyteller.Core.LLM.Role.LLMs'.
proseAgent
  :: forall r
  .  (LLMs r, Members '[PromptStorage, Fail] r)
  => [ModelConfig ProseModel]
  -> Maybe WordCount        -- ^ approximate desired output length
  -> [CharContextBlock]     -- ^ character context blocks
  -> [ContextBlock]         -- ^ branch context file blocks
  -> ExistingContent        -- ^ current content of the file being continued
  -> Instruction
  -> Sem r Prose
proseAgent configs outputHint charContexts contextBlocks (ExistingContent existing) (Instruction instruction) = do
  configsWithPrompt <- getConfigWithPrompt "agent.writer" defaultWriterSystemPrompt configs
  Prompt extraInstructions <- getPrompt "agent.writer.instructions" defaultWriterInstructions

  let userMsg = writerUserMessage contextBlocks charContexts existing extraInstructions instruction outputHint

  response <- queryLLM configsWithPrompt [UserText userMsg]
  return $ Prose $ mconcat [ t | AssistantText t <- response ]

-- | Fallback for @agent.writer@ (the namespace root is implicitly the system
--   prompt/config -- see 'Storyteller.Core.Prompt'), used until an override is committed
--   to the 'Storyteller.Core.Runtime.Prompts' branch.
defaultWriterSystemPrompt :: Prompt
defaultWriterSystemPrompt =
  "You are a creative writing assistant. Write only what is asked. Output only prose, nothing else."

-- | Fallback for @agent.writer.instructions@: standing instructions appended
--   to every writer prompt (house style, voice, recurring constraints), on
--   top of the per-call 'Instruction'. Empty by default — a project opts in
--   by committing an override to the 'Storyteller.Core.Runtime.Prompts' branch.
defaultWriterInstructions :: Prompt
defaultWriterInstructions = ""

-- | Assemble the user-facing prompt from its parts directly, rather than
--   through a named-placeholder template: the section order and headers are
--   fixed by this function, so there's no way for a caller to typo a slot
--   name that silently drops a section. The one piece of free text a
--   project can still override is @extraInstructions@ (see
--   'defaultWriterInstructions'), inserted verbatim at a single fixed point.
writerUserMessage
  :: [ContextBlock]
  -> [CharContextBlock]
  -> T.Text            -- ^ existing file content
  -> T.Text            -- ^ extra standing instructions (may be empty)
  -> T.Text            -- ^ per-call instruction
  -> Maybe WordCount
  -> T.Text
writerUserMessage contextBlocks charContexts existing extraInstructions instruction outputHint =
  mconcat
    [ contextSection
    , charSection
    , existingSection
    , extraInstructionsSection
    , "## Instruction\n\n" <> instruction <> "\n\n"
    , lengthHint
    , "Write only the new text to append. Do not repeat or summarise existing content."
    ]
  where
    contextSection
      | null contextBlocks = ""
      | otherwise =
          "Context files:\n\n"
          <> T.intercalate "\n\n" [ t | ContextBlock t <- contextBlocks ]
          <> "\n\n"

    charSection
      | null charContexts = ""
      | otherwise =
          "Character information:\n\n"
          <> T.intercalate "\n\n" [ t | CharContextBlock t <- charContexts ]
          <> "\n\n"

    existingSection
      | T.null existing = "The file is currently empty.\n\n"
      | otherwise        = "File to continue:\n\n" <> existing <> "\n\n"

    extraInstructionsSection
      | T.null extraInstructions = ""
      | otherwise                = extraInstructions <> "\n\n"

    lengthHint = case outputHint of
      Nothing            -> ""
      Just (WordCount n) -> "Write approximately " <> T.pack (show n) <> " words.\n"

-- | Read the target file's existing content and every other branch file,
--   as plain data — no LLM involved. Requires the target branch's
--   filesystem to be in scope. This is the machinery 'proseAgent' needs fed
--   in; composing the two is the caller's job, e.g.
--   @gatherFileContext layout path >>= \\(existing, ctx) -> proseAgent configs hint chars (extra <> ctx) existing instr@.
--
--   @layout@ is the user-facing bucket-picker ordering
--   ('Storyteller.Writer.Agent.ContextFilter.applyContextLayout') a client
--   may have configured for this call. @[]@ ("no layout configured") falls
--   back to the plain alphabetical order this always used, rather than
--   'applyContextLayout'\'s own @[]@ meaning ("claim nothing, show
--   nothing") — only this caller knows that its own no-layout default is
--   "show everything", so that check has to happen here, not inside
--   'applyContextLayout' itself.
gatherFileContext
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => ContextLayout          -- ^ user-configured bucket ordering, or [] for the default sort
  -> FilePath               -- ^ file to continue
  -> Sem r (ExistingContent, [ContextBlock])
gatherFileContext layout path = do
  unordered   <- listAllFiles @project "/"
  let files = case layout of
        [] -> List.sort unordered
        _  -> applyContextLayout layout unordered
  fileContext <- mapM (readContextFile @project) files
  existing    <- fileExists @project path >>= \case
    True  -> ExistingContent . TE.decodeUtf8 <$> readFile @project path
    False -> return (ExistingContent "")
  return (existing, fileContext)

readContextFile
  :: forall project r
  .  Members '[FileSystemRead project, Fail] r
  => FilePath
  -> Sem r ContextBlock
readContextFile path = do
  bytes <- readFile @project path
  return $ ContextBlock $ "### " <> T.pack path <> "\n\n" <> TE.decodeUtf8 bytes
