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

-- | 'Storyteller.Context.DSL.Rendering' against real DSL output:
--   'renderContext' (the eager, curated bundle), 'renderFileSystem' (the
--   unforced, browsable shape), and the pure floors 'renderText'\/
--   'renderMessages' built off 'renderContext''s result.
module Storyteller.Context.DSL.RenderingSpec (spec) where

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
import Storyteller.Context.DSL.Compile (currentScope)
import Storyteller.Context.DSL.Library (contextLore)
import Storyteller.Context.DSL.Rendering
import Storyteller.Context.DSL.Value

seedBranch :: Text -> [(FilePath, Text)] -> Sem (StoryStorage : TestEffects '[]) ()
seedBranch name files = do
  _ <- createBranch (BranchName name)
  runBranchOpGit @Main (BranchName name)
    (mapM_ (\(path, content) -> runStorage @Main (Ops.addAtom path content)) files)

runDslOn :: BranchName -> Action a -> Sem (StoryStorage : TestEffects '[]) a
runDslOn bname act = resolveBranch bname >>= \case
  Nothing -> fail ("branch not found: " <> T.unpack (unBranchName bname))
  Just h  -> fst <$> Core.runStoreT h (runAction act (buildContextLibrary Map.empty))

describeMessage :: LLM.Message m -> (LLM.MessageDirection, Text)
describeMessage msg@(LLM.UserText t)      = (LLM.messageDirection msg, t)
describeMessage msg@(LLM.AssistantText t) = (LLM.messageDirection msg, t)
describeMessage msg                       = (LLM.messageDirection msg, "<unsupported in this test>")

spec :: Spec
spec = do
  renderContextSpec
  renderFileSystemSpec

renderContextSpec :: Spec
renderContextSpec = describe "renderContext / renderText / renderMessages" $ do
  it "renderText concatenates every reachable message's own content, ignoring role" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runDslOn (BranchName "main") (renderText <$> (renderContext =<< contextLore)))
    `shouldBe` Right
      "## Story background\n\n## lore/notes.md\n\na hand-authored note"

  it "renderMessages preserves role, one LLM.Message per DSL Message" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runDslOn (BranchName "main")
        (map describeMessage . (renderMessages :: Context -> [LLM.Message ProseModel])
          <$> (renderContext =<< contextLore)))
    `shouldBe` Right
      [ (LLM.User, "## Story background")
      , (LLM.User, "## lore/notes.md")
      , (LLM.User, "<context-file path=\"lore/notes.md\">\na hand-authored note\n</context-file>")
      ]

  it "namedChild reaches the per-file entry contextLore also exports" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runDslOn (BranchName "main") (do
        ctx <- renderContext =<< contextLore
        case namedChild "lore/notes.md" ctx of
          Nothing    -> fail "expected a lore/notes.md entry"
          Just child -> pure (renderText child)))
    `shouldBe` Right "## lore/notes.md\n\na hand-authored note"

-- | Deliberately run against 'currentScope' (the raw branch tree), not a
--   composed library definition like 'contextLore' -- 'Provenance' is
--   stamped only where a leaf comes straight from
--   'Storyteller.Context.DSL.Compile.treeValueOfCommit', and is lost the
--   moment content passes through 'Storyteller.Context.DSL.Compile.runStmts'\/
--   'Storyteller.Context.DSL.Compile.mkValue' (a composed, multi-statement
--   node has no single sensible provenance to assign -- see
--   'Storyteller.Context.DSL.Rendering''s own module haddock). So
--   'renderFileSystem' is honestly only meaningful on an untouched tree or
--   a bare @read@ result, not on an arbitrary definition's already-
--   composed output.
renderFileSystemSpec :: Spec
renderFileSystemSpec = describe "renderFileSystem / listDeferred / readRef" $ do
  it "lists every provenance-carrying entry without forcing any content" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runDslOn (BranchName "main") (do
        fsv <- renderFileSystem =<< currentScope
        pure (map (provPath . crSource) (listDeferred fsv))))
    `shouldBe` Right ["lore/notes.md"]

  it "readRef forces exactly the referenced entry's own content" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runDslOn (BranchName "main") (do
        fsv <- renderFileSystem =<< currentScope
        case listDeferred fsv of
          [ref] -> messageText . ciMessage <$> readRef ref
          refs  -> fail ("expected exactly one ref, got " <> show (length refs))))
    `shouldBe` Right "a hand-authored note"
