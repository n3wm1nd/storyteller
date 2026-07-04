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
import Runix.LLM (LLM, queryLLM)
import UniversalLLM (Message(..), ModelConfig(..))

import Storyteller.Writer.Agent (Instruction(..), Prose(..), CharContextBlock(..), ContextBlock(..), ExistingContent(..), WordCount(..))

import Prelude hiding (readFile)

-- | Ask the LLM to produce new prose given fully assembled context.
--   No filesystem access — all inputs are explicit.
--   This is the composition point for cross-cutting concerns: style guides,
--   persona, tone constraints, etc. can be added here as new effects.
proseAgent
  :: forall model r
  .  Members '[LLM model, Fail] r
  => [ModelConfig model]
  -> Maybe WordCount        -- ^ approximate desired output length
  -> [CharContextBlock]     -- ^ character context blocks
  -> [ContextBlock]         -- ^ branch context file blocks
  -> ExistingContent        -- ^ current content of the file being continued
  -> Instruction
  -> Sem r Prose
proseAgent configs outputHint charContexts contextBlocks (ExistingContent existing) (Instruction instruction) = do
  let contextSection
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
        | otherwise       = "File to continue:\n\n" <> existing <> "\n\n"

      lengthHint = case outputHint of
        Nothing            -> ""
        Just (WordCount n) -> "Write approximately " <> T.pack (show n) <> " words.\n"

      userMsg = contextSection
             <> charSection
             <> existingSection
             <> "Instruction: " <> instruction <> "\n\n"
             <> lengthHint
             <> "Write only the new text to append. Do not repeat or summarise existing content."

  response <- queryLLM @model configs [UserText userMsg]
  return $ Prose $ mconcat [ t | AssistantText t <- response ]

-- | Read the target file's existing content and every other branch file,
--   as plain data — no LLM involved. Requires the target branch's
--   filesystem to be in scope. This is the machinery 'proseAgent' needs fed
--   in; composing the two is the caller's job, e.g.
--   @gatherFileContext path >>= \\(existing, ctx) -> proseAgent configs hint chars (extra <> ctx) existing instr@.
gatherFileContext
  :: forall project r
  .  Members '[FileSystem project, FileSystemRead project, Fail] r
  => FilePath               -- ^ file to continue
  -> Sem r (ExistingContent, [ContextBlock])
gatherFileContext path = do
  files       <- List.sort <$> listAllFiles @project "/"
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
