{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
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
import Test.Hspec

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail)

import qualified UniversalLLM as LLM

import qualified Storage.Ops as Ops
import Storyteller.Core.Git (BranchOp, runBranchAndFS, runBranchOpGit, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Core.Context (ContextRow, ContextStorage, buildContextLibrary, runContextValue)
import Storyteller.Core.ContentEffects (BranchResolve)
import Storyteller.Core.LLM.Role (ProseModel)
import Storyteller.Context.DSL.Compile (Library, currentScope)
import Storyteller.Context.DSL.Library (contextLore)
import Storyteller.Context.DSL.Rendering
import Storyteller.Context.DSL.Value

seedBranch :: Text -> [(FilePath, Text)] -> Sem (StoryStorage : TestEffects '[]) ()
seedBranch name files = do
  _ <- createBranch (BranchName name)
  runBranchOpGit @Main (BranchName name)
    (mapM_ (\(path, content) -> runStorage @Main (Ops.addAtom path content)) files)

runDslOn
  :: forall a
  .  BranchName
  -> (forall r. Members '[BranchOp Main, BranchResolve, ContextStorage, Fail] r => Action (ContextRow Main r) a)
  -> Sem (StoryStorage : TestEffects '[]) a
runDslOn bname act = runBranchAndFS @Main bname (runContextValue @Main act)

-- | No overrides are ever staged in this spec -- just the compiled-in
--   defaults, same as 'buildContextLibrary' would build from an empty
--   override map.
emptyLib :: forall r. Members '[BranchResolve, Fail] r => Library (ContextRow Main r)
emptyLib = buildContextLibrary @Main Map.empty

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
      runDslOn (BranchName "main") (renderText <$> (renderContext =<< contextLore @Main emptyLib)))
    `shouldBe` Right
      "## Story background\n\n## lore/notes.md\n\na hand-authored note"

  it "renderMessages preserves role, one LLM.Message per DSL Message" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runDslOn (BranchName "main")
        (map describeMessage . (renderMessages :: Context -> [LLM.Message ProseModel])
          <$> (renderContext =<< contextLore @Main emptyLib)))
    `shouldBe` Right
      [ (LLM.User, "## Story background")
      , (LLM.User, "## lore/notes.md")
      , (LLM.User, "<context-file path=\"lore/notes.md\">\na hand-authored note\n</context-file>")
      ]

  it "namedChild reaches the per-file entry contextLore also exports" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runDslOn (BranchName "main") (do
        ctx <- renderContext =<< contextLore @Main emptyLib
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
        fsv <- renderFileSystem =<< currentScope @Main
        pure (map (provPath . crSource) (listDeferred fsv))))
    `shouldBe` Right ["lore/notes.md"]

  it "readRef forces exactly the referenced entry's own content" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runDslOn (BranchName "main") (do
        fsv <- renderFileSystem =<< currentScope @Main
        case listDeferred fsv of
          [ref] -> messageText . ciMessage <$> readRef @Main ref
          refs  -> fail ("expected exactly one ref, got " <> show (length refs))))
    `shouldBe` Right "a hand-authored note"
