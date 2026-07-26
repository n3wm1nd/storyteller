{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | User-defined agents ('Storyteller.Writer.Agent.Custom'): that the two
--   naming conventions the whole feature rests on actually resolve, and
--   that the starter template a new agent is seeded with really is
--   @writeAgent@'s own context expressed in DSL.
--
--   Neither is checkable by reading the code: both are string conventions
--   spanning three layers (a dotted 'Storyteller.Context.DSL.AST.Name', a
--   path on the @contexts@ branch, and the frontend's own discovery glob),
--   and the compiler sees none of it -- exactly the "compiles fine, still
--   wrong" shape CLAUDE.md warns about. The seed template especially: it
--   is *text*, compiled at runtime against whatever library
--   'Storyteller.Core.Context.buildContextLibrary' built, so "does
--   @context.custom@ still name real definitions" has no static answer.
module Storyteller.CustomAgentSpec (spec) where

import Test.Hspec

import qualified Data.Text as T
import Polysemy (run)
import UniversalLLM (Message(..))

import Storyteller.Context.DSL.Rendering (renderContext, renderText)
import Storyteller.Core.Context (resolveContext1, runContextValue, setContextOverride)
import Storyteller.Core.Git (runBranchAndFS, runStorage)
import Storyteller.Core.LLM.Role (ProseModel)
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types (BranchName(..))
import qualified Storage.Ops as Ops
import qualified Storage.Tick as Tick

import Server.Core.Branch (Main)
import Server.TestStack
import Storyteller.Writer.Agent (Instruction(..), Prompt(..))
import Storyteller.Writer.Agent.Custom
  (buildCustomMessages, customContextName, customPromptKey)

-- | Resolve a @context.*@ slot at arity 1 against a small, real story
--   branch and flatten it to text -- the same
--   'resolveContext1'-then-render path 'Storyteller.Writer.Agent.Custom.customAgent'
--   itself takes, so nothing here can pass while the real call fails.
resolveText :: [(T.Text, T.Text)] -> T.Text -> FilePath -> Either String T.Text
resolveText overrides name path = run . testStack $ do
  _ <- createBranch (BranchName "story")
  runBranchAndFS @Main (BranchName "story") $ do
    _ <- runStorage @Main (Ops.addAtom "style.md" "write in short sentences")
    _ <- runStorage @Main (Ops.addAtom "lore/world.md" "the world is flat")
    _ <- runStorage @Main (Ops.addAtom "chapters/ch1.md" "the first chapter")
    _ <- runStorage @Main (Ops.addAtom "notes.md" "a loose note")
    -- The file being written, as a real prompt/atom exchange -- what
    -- @readconversation@ replays as user/assistant turns. Written in
    -- production's own order (the file exists, *then* a turn is taken):
    -- 'Storage.Tick.fileTicksOf' is lifetime-scoped to when the path is
    -- actually present in the tree, so a prompt tick minted before the
    -- file's first atom belongs to no file and is correctly invisible
    -- here -- a fixture shortcut that skips the first atom silently tests
    -- an arrangement no real chapter is ever in.
    _ <- runStorage @Main (Ops.addAtom "chapters/ch2.md" "The ship came in at dusk.")
    _ <- runStorage @Main (Tick.storeAs (Prompt "chapters/ch2.md" "open on the harbour"))
    _ <- runStorage @Main (Ops.addAtom "chapters/ch2.md" "Gulls turned over the water.")
    mapM_ (uncurry setContextOverride) overrides
    v <- resolveContext1 @Main name (T.pack path)
    renderText <$> runContextValue @Main (renderContext v)

spec :: Spec
spec = do

  describe "naming conventions" $ do
    -- Both halves of an agent are addressed by one slug, and each dotted
    -- name doubles as a branch path (dots -> slashes) -- the frontend
    -- enumerates agents by globbing exactly these paths, so a change here
    -- silently unlists every existing agent rather than failing loudly.
    it "maps a slug to its context slot and prompt key" $ do
      customContextName "critic" `shouldBe` "context.custom.critic"
      customPromptKey "critic" `shouldBe` "agent.custom.critic"

  describe "context.custom (the seed template)" $ do

    -- The claim the template exists to make: a brand-new agent starts out
    -- seeing what the built-in writer sees. Asserted piece by piece
    -- against real branch content rather than as one golden string, so a
    -- reworded banner elsewhere doesn't fail this while a genuinely
    -- missing section still does.
    it "includes the writer's own background: lore, chapters, loose notes" $ do
      let out = resolveText [] "context.custom" "chapters/ch2.md"
      out `shouldSatisfy` either (const False) (T.isInfixOf "the world is flat")
      out `shouldSatisfy` either (const False) (T.isInfixOf "the first chapter")
      out `shouldSatisfy` either (const False) (T.isInfixOf "a loose note")

    -- The one piece `context.writer` does *not* carry: writeAgent splices
    -- the style guide into its own system prompt instead, and a custom
    -- agent has no such splice — so a template that just delegated to
    -- `context.writer` would silently drop it from every user-defined
    -- agent. This is the assertion that keeps that true.
    it "includes the style guide, which context.writer itself does not" $ do
      resolveText [] "context.writer" "chapters/ch2.md"
        `shouldSatisfy` either (const False) (not . T.isInfixOf "write in short sentences")
      resolveText [] "context.custom" "chapters/ch2.md"
        `shouldSatisfy` either (const False) (T.isInfixOf "write in short sentences")

    -- The half that isn't just `context.writer`: the file being written
    -- comes back as its own conversation, not as another background file.
    it "replays the current file as conversation turns" $ do
      let out = resolveText [] "context.custom" "chapters/ch2.md"
      out `shouldSatisfy` either (const False) (T.isInfixOf "open on the harbour")
      out `shouldSatisfy` either (const False) (T.isInfixOf "Gulls turned over the water.")

    -- `context.writer`'s own path exclusion has to survive being called
    -- through the template: the file under the cursor is present exactly
    -- once (as conversation), never a second time as background prose.
    it "does not also include the current file as background" $ do
      let occurrences = either (const (-1)) (length . T.breakOnAll "Gulls turned over the water.")
      occurrences (resolveText [] "context.custom" "chapters/ch2.md") `shouldBe` 1

  describe "a project's own agent program" $ do

    -- The end-to-end convention check: text committed at
    -- `context/custom/critic.dsl` (staged here exactly as a real request
    -- stages a per-call override -- same map, same compile path) is what
    -- the agent runs, at arity 1, with the open file's path bound.
    it "resolves at its dotted name, with the file path as its parameter" $
      resolveText [("context.custom.critic", "path:\n  \"reviewing:\"\n  path\n")]
        (customContextName "critic") "chapters/ch2.md"
        `shouldBe` Right "reviewing:\n\nchapters/ch2.md"

    -- A project narrowing the template is the expected first edit, so the
    -- template's own names have to still resolve from a project's file.
    it "can call the seed template by name and add to it" $ do
      let out = resolveText [("context.custom.critic", "path:\n  context.custom path\n  \"be harsh\"\n")]
                  (customContextName "critic") "chapters/ch2.md"
      out `shouldSatisfy` either (const False) (T.isInfixOf "the world is flat")
      out `shouldSatisfy` either (const False) (T.isInfixOf "be harsh")

    -- No compiled-in fallback: an agent is its files, so a slug with no
    -- program is an error, not an agent running on empty context (which
    -- would look like a working agent producing bad output).
    it "fails loudly when the slug names no program at all" $
      resolveText [] (customContextName "nosuch") "chapters/ch2.md"
        `shouldSatisfy` either (const True) (const False)

  describe "buildCustomMessages" $ do

    it "puts the instruction last, after the program's own stream" $
      buildCustomMessages @ProseModel [UserText "background"] "" (Instruction "write it")
        `shouldBe` [UserText "background", UserText "write it"]

    it "places pinned content between the program and the instruction" $
      buildCustomMessages @ProseModel [UserText "background"] "pinned bit" (Instruction "write it")
        `shouldBe` [UserText "background", UserText "pinned bit", UserText "write it"]

    -- An empty pinned selection is the norm, not an edge case -- it must
    -- not cost an empty user message (which some providers reject outright
    -- and every provider bills for).
    it "drops pinned content entirely when there is none" $
      buildCustomMessages @ProseModel [] "" (Instruction "write it")
        `shouldBe` [UserText "write it"]
