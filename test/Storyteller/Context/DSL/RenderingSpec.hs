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
import qualified Data.Text as T
import Test.Hspec

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail)

import qualified UniversalLLM as LLM

import qualified Storage.Ops as Ops
import Storyteller.Core.Branch (Branches)
import Storyteller.Core.Git (BranchOp, runBranchAndFS, runBranchOpGit, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Core.Context (ContextRow, ContextStorage, buildContextLibrary, runContextValue)
import Storyteller.Core.LLM.Role (ProseModel)
import Storyteller.Context.DSL.Compile (Library, currentScope)
import Storyteller.Context.DSL.Library (contextLore, contextCharacter)
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
  -> (forall r. Members '[BranchOp Main, Branches, ContextStorage, Fail] r => Action (ContextRow r) a)
  -> Sem (StoryStorage : TestEffects '[]) a
runDslOn bname act = runBranchAndFS @Main bname (runContextValue @Main act)

-- | No overrides are ever staged in this spec -- just the compiled-in
--   defaults, same as 'buildContextLibrary' would build from an empty
--   override map.
emptyLib :: forall r. Members '[Branches, BranchOp Main, Fail] r => Library (ContextRow r)
emptyLib = fst (buildContextLibrary @Main Map.empty)

describeMessage :: LLM.Message m -> (LLM.MessageDirection, Text)
describeMessage msg@(LLM.UserText t)      = (LLM.messageDirection msg, t)
describeMessage msg@(LLM.AssistantText t) = (LLM.messageDirection msg, t)
describeMessage msg                       = (LLM.messageDirection msg, "<unsupported in this test>")

spec :: Spec
spec = do
  renderContextSpec
  renderFileSystemSpec
  contextAllMessagesSpec
  monoidSpec

-- | The distinction between 'renderText'\/'renderMessages' (this node's own
--   default) and 'contextAllMessages' (own default plus each child's), on
--   the shape that actually makes them differ.
--
--   This is a regression guard for a real bug, not a completeness exercise.
--   'Storyteller.Context.DSL.Library.contextCharacterDef''s @"full"@ bucket
--   is a @for f in **\/*: as f: read f@ loop -- every file lands in a named
--   child and the bucket's own default stays empty. A caller reaching for
--   that bucket with 'renderText' gets the empty string and no error, which
--   is exactly how 'Storyteller.Writer.Agent.AskCharacter' briefly stopped
--   sending a character's own files while every existing test still passed.
--   The fixture below is that shape deliberately: content only in children.
contextAllMessagesSpec :: Spec
contextAllMessagesSpec = describe "contextAllMessages vs renderText" $ do
  -- Deliberately NOT contextLore: its per-file loop folds each entry's
  -- content into its own top-level default as well as exporting it by name
  -- (see 'renderText''s Haddock), so renderText finds the content there and
  -- the two traversals agree -- a fixture that would pass with the bug
  -- present. context.character's "full" is the shape that actually differs:
  -- the loop *is* the bucket's body, so its own default is empty.
  it "reads context.character's \"full\" bucket, whose own default is empty and whose files are all children" $
    run (testStack $ do
      seedBranch "main" []
      seedBranch "character/mira"
        [ ("sheet.md", "Mira, a locksmith")
        , ("history.md", "she left the city in winter")
        , ("voice.md", "clipped, dry")
        ]
      runDslOn (BranchName "main") (do
        scope   <- currentScope
        charVal <- contextCharacter scope emptyLib "mira"
        ctx     <- renderContext charVal
        case namedChild "full" ctx of
          Nothing   -> fail "no \"full\" bucket"
          Just full -> pure ( map messageText (contextAllMessages full)
                            , renderText full )))
    `shouldSatisfy` \case
        Right (allMsgs, ownOnly) ->
          -- Both non-sheet files reachable through the children...
          all (\t -> any (T.isInfixOf t) allMsgs) ["left the city", "clipped, dry"]
          -- ...and none of it through the bucket's own default, which is
          -- what made the regression silent rather than loud.
          && T.null (T.strip ownOnly)
        Left _ -> False

  it "does not walk grandchildren -- one level only, matching valueAllMessages" $
    let leaf t   = Node [ContextItem (User t) defaultMeta] []
        tree     = Node [ContextItem (User "own") defaultMeta]
                        [("child", Node [ContextItem (User "childOwn") defaultMeta]
                                        [("grandchild", leaf "deep")])]
    in map messageText (contextAllMessages tree) `shouldBe` ["own", "childOwn"]

-- | 'RenderedContext''s 'Semigroup'\/'Monoid' instance -- plain
--   concatenation, content then entries, in that order. What
--   'Server.Writer.File.chatWriter' uses to fold its independently-
--   resolved slots (@context.lore@, chapters, @context.other@) into one
--   stream, and separately to
--   fold each @fcPinnedPrograms@ entry's own rendered result in alongside
--   the client's plain pinned items.
monoidSpec :: Spec
monoidSpec = describe "RenderedContext's Semigroup/Monoid instance" $ do
  it "<> concatenates content and entries in order, left then right" $
    let a = Node ["a1", "a2"] [("x", Node ["ax"] [])]
        b = Node ["b1"]       [("y", Node ["by"] [])]
    in (a <> b) `shouldBe` Node ["a1", "a2", "b1"] [("x", Node ["ax"] []), ("y", Node ["by"] [])]

  it "mempty is a genuine identity on both sides" $ do
    let a = Node ["a1"] [("x", Node ["ax"] [])] :: RenderedContext Text
    (mempty <> a) `shouldBe` a
    (a <> mempty) `shouldBe` a

  it "mconcat folds a list the same way repeated <> would" $
    let nodes = [Node ["1"] [], Node ["2"] [], Node ["3"] []] :: [RenderedContext Text]
    in mconcat nodes `shouldBe` Node ["1", "2", "3"] []

renderContextSpec :: Spec
renderContextSpec = describe "renderContext / renderText / renderMessages" $ do
  it "renderText concatenates every reachable message's own content, ignoring role" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runDslOn (BranchName "main") (renderText <$> (renderContext =<< (currentScope >>= \s -> contextLore s emptyLib))))
    `shouldBe` Right
      "## Story background\n\n## lore/notes.md\n\na hand-authored note"

  it "renderMessages preserves role, one LLM.Message per DSL Message" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runDslOn (BranchName "main")
        (map describeMessage . (renderMessages :: Context -> [LLM.Message ProseModel])
          <$> (renderContext =<< (currentScope >>= \s -> contextLore s emptyLib))))
    `shouldBe` Right
      [ (LLM.User, "## Story background")
      , (LLM.User, "## lore/notes.md")
      , (LLM.User, "<context-file path=\"lore/notes.md\">\na hand-authored note\n</context-file>")
      ]

  it "namedChild reaches the per-file entry contextLore also exports" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runDslOn (BranchName "main") (do
        ctx <- renderContext =<< (currentScope >>= \s -> contextLore s emptyLib)
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
        scope <- currentScope
        fsv   <- renderFileSystem scope
        case listDeferred fsv of
          [ref] -> messageText . ciMessage <$> readRef scope ref
          refs  -> fail ("expected exactly one ref, got " <> show (length refs))))
    `shouldBe` Right "a hand-authored note"
