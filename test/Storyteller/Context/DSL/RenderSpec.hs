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

import Storyteller.Core.LLM.Role (ProseModel)
import Storyteller.Context.DSL.Library (contextChapters, contextLore)
import Storyteller.Context.DSL.Render (valueBlocks, valueMessages)
import Storyteller.Context.DSL.Value
import Storyteller.Writer.Agent (ContextBlock(..))

seedBranch :: Text -> [(FilePath, Text)] -> Sem (StoryStorage : TestEffects '[]) ()
seedBranch name files = do
  _ <- createBranch (BranchName name)
  runBranchOpGit @Main (BranchName name)
    (mapM_ (\(path, content) -> runStorage @Main (Ops.addAtom path content)) files)

runDslOn :: BranchName -> Action a -> Sem (StoryStorage : TestEffects '[]) a
runDslOn bname act = resolveBranch bname >>= \case
  Nothing -> fail ("branch not found: " <> T.unpack (unBranchName bname))
  Just h  -> fst <$> Core.runStoreT h (runAction act)

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

valueMessagesSpec :: Spec
valueMessagesSpec = describe "valueMessages" $
  it "preserves contextChapters' own User/Assistant pairing, in natural order, across two chapters" $
    run (testStack $ do
      seedBranch "main"
        [ ("chapters/ch11.md", "chapter eleven prose")
        , ("chapters/ch2.md", "chapter two prose")
        ]
      runDslOn (BranchName "main")
        (map describeMessage <$> (valueMessages =<< contextChapters :: Action [LLM.Message ProseModel])))
    `shouldBe` Right
      [ (LLM.User,      "## Chapter: chapters/ch2.md")
      , (LLM.Assistant, "chapter two prose")
      , (LLM.User,      "## Chapter: chapters/ch11.md")
      , (LLM.Assistant, "chapter eleven prose")
      ]

valueBlocksSpec :: Spec
valueBlocksSpec = describe "valueBlocks" $
  it "flattens contextLore into fenced ContextBlocks, one per file" $
    run (testStack $ do
      seedBranch "main"
        [ ("lore/places/tavern.md", "the tavern")
        , ("lore/notes.md", "a note")
        ]
      runDslOn (BranchName "main") (valueBlocks =<< contextLore))
    `shouldBe` Right
      [ ContextBlock "<context-file path=\"lore/notes.md\">\na note\n</context-file>"
      , ContextBlock "<context-file path=\"lore/places/tavern.md\">\nthe tavern\n</context-file>"
      ]
