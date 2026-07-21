{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'Storyteller.Context.DSL.Render' against real DSL output -- in
--   particular, that 'valueMessages' preserves 'contextChapters''s own
--   User\/Assistant pairing across the translation into
--   'UniversalLLM.Message', not just at the DSL layer (already checked in
--   "Storyteller.Context.DSL.CompileSpec") but all the way through to what
--   an agent call actually receives.
module Storyteller.Context.DSL.RenderSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail)

import Runix.Git (Git)
import qualified UniversalLLM as LLM

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Core.Git (runBranchOpGit, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (Branch(..), BranchName(..), TickId(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Core.Context (buildContextLibrary)
import Storyteller.Core.LLM.Role (ProseModel)
import Storyteller.Context.DSL.Library (contextChapters, contextLore)
import qualified Storyteller.Context.DSL.Render as Render
import Storyteller.Context.DSL.Value
import Storyteller.Writer.Agent (ContextBlock(..))

seedBranch :: Text -> [(FilePath, Text)] -> Sem (StoryStorage : TestEffects '[]) ()
seedBranch name files = do
  _ <- createBranch (BranchName name)
  runBranchOpGit @Main (BranchName name)
    (mapM_ (\(path, content) -> runStorage @Main (Ops.addAtom path content)) files)

-- | Runs against 'Storyteller.Context.DSL.Library.defaultLibrarySource',
--   not an empty library -- 'contextChapters'\/'contextLore' only resolve
--   at all because 'Storyteller.Context.DSL.Library.chapterEntry'\/
--   'Storyteller.Context.DSL.Library.loreEntry' are in it.
runDslOn :: BranchName -> Action a -> Sem (StoryStorage : TestEffects '[]) a
runDslOn bname act = resolveBranch bname >>= \case
  Nothing -> fail ("branch not found: " <> T.unpack (unBranchName bname))
  Just h  -> fst <$> Core.runStoreT h (runAction act (buildContextLibrary Map.empty))

-- | 'LLM.Message' has no 'Eq' -- compare on 'LLM.messageDirection' plus
--   the rendered text, which is everything a real caller ('writeAgent',
--   via 'UniversalLLM.queryLLM') can see or would ever assert on.
describeMessage :: LLM.Message m -> (LLM.MessageDirection, Text)
describeMessage msg@(LLM.UserText t)      = (LLM.messageDirection msg, t)
describeMessage msg@(LLM.AssistantText t) = (LLM.messageDirection msg, t)
describeMessage msg                       = (LLM.messageDirection msg, "<unsupported in this test>")

spec :: Spec
spec = do
  valueMessagesSpec
  valueBlocksSpec

-- | Reads a definition's own @valueDefault@ directly, not
--   'Storyteller.Context.DSL.Render.valueAllMessages'
--   (@valueMessages@\/@valueBlocks@'s own shared walk): now that
--   'contextChapters'\/'contextLore' are self-describing (their own
--   default already carries the full, already-framed content -- see
--   'contextLore''s own Haddock), also walking 'valueEntries' would
--   double-count the same messages.
ownMessages :: Value -> Action [Message]
ownMessages = valueDefault

valueMessagesSpec :: Spec
valueMessagesSpec = describe "valueMessages (via contextChapters' own default)" $
  it "preserves contextChapters' own User/Assistant pairing, in natural order, across two chapters" $
    run (testStack $ do
      seedBranch "main"
        [ ("chapters/ch11.md", "chapter eleven prose")
        , ("chapters/ch2.md", "chapter two prose")
        ]
      runDslOn (BranchName "main")
        (map describeMessage . map Render.dslMessageToLLM <$> (ownMessages =<< contextChapters) :: Action [(LLM.MessageDirection, Text)]))
    `shouldBe` Right
      [ (LLM.User,      "## Chapters written so far")
      , (LLM.User,      "## Chapter: chapters/ch2.md")
      , (LLM.Assistant, "chapter two prose")
      , (LLM.User,      "## Chapter: chapters/ch11.md")
      , (LLM.Assistant, "chapter eleven prose")
      ]

valueBlocksSpec :: Spec
valueBlocksSpec = describe "valueBlocks (via contextLore's own default)" $
  it "flattens contextLore into a heading plus one fenced ContextBlock per file" $
    run (testStack $ do
      seedBranch "main"
        [ ("lore/places/tavern.md", "the tavern")
        , ("lore/notes.md", "a note")
        ]
      runDslOn (BranchName "main") (map Render.messageToBlock <$> (ownMessages =<< contextLore)))
    `shouldBe` Right
      [ ContextBlock "## Story background"
      , ContextBlock "## lore/notes.md"
      , ContextBlock "<context-file path=\"lore/notes.md\">\na note\n</context-file>"
      , ContextBlock "## lore/places/tavern.md"
      , ContextBlock "<context-file path=\"lore/places/tavern.md\">\nthe tavern\n</context-file>"
      ]
