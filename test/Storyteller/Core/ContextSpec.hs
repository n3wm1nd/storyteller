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

-- | 'Storyteller.Core.Context.ContextStorage' -- the Context DSL's
--   'Storyteller.Core.Prompt.PromptStorage' equivalent. Checks the pure
--   parse-only decision ('resolveOverrideDefinition') directly, then
--   'resolveContext0'\/'resolveContext1' end to end against both
--   interpreters: a missing override falls back to the compiled-in default
--   unchanged, and a real committed override on the dedicated 'Contexts'
--   branch actually takes over -- run from the *caller's* ambient branch
--   position (not the Contexts branch itself), the same "whatever I'm
--   already in" contract every other Context DSL definition gets.
--
--   'resolveContext0'\/'resolveContext1' no longer take a caller-supplied
--   Haskell default (see 'Storyteller.Core.Context.buildContextLibrary'
--   and 'Storyteller.Context.DSL.Compile.runNamed''s own Haddocks) -- a
--   name only ever resolves against 'defaultLibraryOrder''s real compiled
--   graph now, so these tests exercise real slots (@context.style@,
--   @context.character@) rather than the test-local
--   @context.greeting@\/@context.greeting1@ stand-ins the previous
--   fallback-taking design needed.
module Storyteller.Core.ContextSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail)

import qualified UniversalLLM as LLM

import qualified Storage.Ops as Ops
import Storyteller.Core.LLM.Role (ProseModel)
import Storyteller.Core.Context
  ( contextsBranchName, setContextOverride, interpretContextStorageFS, interpretContextStorageMap
  , resolveOverrideDefinition, ContextRow, ContextStorage, resolveContext0, resolveContext1, resolveAdhoc0, runContextValue
  , buildContextLibrary, getContextOverrides )
import Storyteller.Core.ContentEffects (BranchResolve)
import Storyteller.Core.Branch (Branches)
import Storyteller.Core.Git (BranchOp, runBranchAndFS, runBranchOpGit, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Context.DSL.AST (Name)
import Storyteller.Context.DSL.Compile (Library)
import Storyteller.Context.DSL.Rendering (renderContext, renderText, renderMessages, namedChild)
import Storyteller.Context.DSL.Value
import Storyteller.Writer.Presence (enters)
import Storyteller.Writer.Types (Character(..))

seedBranch :: Text -> [(FilePath, Text)] -> Sem (StoryStorage : TestEffects '[]) ()
seedBranch name files = do
  _ <- createBranch (BranchName name)
  runBranchOpGit @Main (BranchName name)
    (mapM_ (\(path, content) -> runStorage @Main (Ops.addAtom path content)) files)

spec :: Spec
spec = do
  resolveOverrideDefinitionSpec
  resolveContext0Spec
  resolveContext1Spec
  resolveAdhoc0Spec
  clientSubmittedContextProgramSpec
  frontendSynthesizedProgramShapeSpec
  buildContextLibrarySpec

-- | The actual point of the whole override mechanism, end to end, against
--   the real production definition and the real rendering pipeline --
--   every other test in this module proves the *mechanism* works against a
--   toy @context.greeting@ default; this is what a client sending a DSL
--   program with its own request (@fcContext@ on
--   'Server.Writer.File.Protocol.ChatWriter', staged via
--   'setContextOverride' exactly the way
--   'Server.Writer.File.chatWriter' does) actually changes: not just "some
--   binding resolves to different text," but the literal messages
--   'Storyteller.Writer.Agent.Write.writeAgent' would receive, once
--   rendered ('Storyteller.Context.DSL.Rendering.renderContext' ->
--   'Storyteller.Context.DSL.Rendering.renderText'\/'Storyteller.Context.DSL.Rendering.renderMessages').
--   Without a client program, resolving @context.writer@ against seeded
--   lore falls through to the compiled-in default
--   ('Storyteller.Context.DSL.Library.contextWriter'); with one staged,
--   the client's own program wins completely, discarding that lore.
clientSubmittedContextProgramSpec :: Spec
clientSubmittedContextProgramSpec = describe "a client-submitted context.writer program, end to end" $ do
  it "with no client program, resolves to the compiled-in default (real lore included)" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runBranchAndFS @Main (BranchName "main") $ do
        writerV <- resolveContext1 @Main "context.writer" "target.md"
        renderText <$> runContextValue @Main (renderContext writerV))
    `shouldBe` Right "## Story background\n\n## lore/notes.md\n\na hand-authored note\n\n## Chapters written so far\n\n## Other notes"

  it "a client program staged via setContextOverride replaces the default completely, seeded lore included" $
    run (testStack $ do
      seedBranch "main" [("lore/notes.md", "a hand-authored note")]
      runBranchAndFS @Main (BranchName "main") $ do
        setContextOverride "context.writer" "path:\n  \"a client-submitted override, replacing everything\"\n"
        writerV <- resolveContext1 @Main "context.writer" "target.md"
        ctx <- runContextValue @Main (renderContext writerV)
        pure (renderText ctx, map describeMessage (renderMessages ctx :: [LLM.Message ProseModel])))
    `shouldBe` Right
      ( "a client-submitted override, replacing everything"
      , [(LLM.User, "a client-submitted override, replacing everything")]
      )

-- | The frontend's own half of this contract, pinned from the backend
--   side: @frontend\/src\/lib\/dslCompose.ts@'s @synthesizeProgram@\/
--   @composeSendProgram@ produce exactly this shape (a leading @path:@
--   parameter line, everything else indented under it) for the same
--   reason 'clientSubmittedContextProgramSpec' stages one by hand --
--   'resolveOverrideDefinition' silently discards any override whose
--   parsed arity doesn't match @context.writer@'s own (1), falling back
--   to the compiled-in default with no error surfaced anywhere. A drift
--   between what the frontend actually emits and what this test embeds
--   is a real risk (two independent renderings of the same contract, in
--   two different languages) -- this is the guard against that drift
--   silently regressing, the same way 'contextCharacterBlurbOverrideSpec'
--   guards @character.blurb@'s own composition path.
frontendSynthesizedProgramShapeSpec :: Spec
frontendSynthesizedProgramShapeSpec =
  describe "the frontend's own synthesized context.writer override, staged verbatim" $
    it "parses at arity 1 and actually overrides context.writer, excluding path from chapters and including an active character" $
      run (testStack $ do
        seedBranch "main"
          [ ("lore/notes.md", "a hand-authored note")
          , ("chapters/ch2.md", "chapter two prose")
          ]
        _ <- createBranch (BranchName "character/aria")
        runBranchOpGit @Main (BranchName "character/aria")
          (runStorage @Main (Ops.addAtom "sheet.md" "# Aria\n\nA wandering rogue."))
        runBranchAndFS @Main (BranchName "main") $ do
          _ <- enters @Main "chapters/ch2.md" (Character (BranchName "character/aria"))
          setContextOverride "context.writer" frontendProgram
          writerV <- resolveContext1 @Main "context.writer" "chapters/ch2.md"
          runContextValue @Main $ do
            rc <- renderContext writerV
            pure (renderText rc, renderText <$> namedChild "aria" rc))
      `shouldBe` Right
        ( "## Story background\n\n## lore/notes.md\n\na hand-authored note"
        , Just "Aria: A wandering rogue."
        )
  where
    -- Exactly `synthesizeProgram`'s own output shape for
    -- `{ baseline: { lore: true, chapters: true, style: false },
    --    characters: [], extraFiles: [] }` -- see dslCompose.ts.
    frontendProgram = T.unlines
      [ "path:"
      , "  context.lore"
      , "  in (context.chapters | exclude(path)):"
      , "    for f in **/*: read f"
      , "  for c in (charactersin path):"
      , "    as c: context.character c"
      ]

-- | 'LLM.Message' has no 'Eq' -- compare on 'LLM.messageDirection' plus
--   the rendered text, same pattern
--   "Storyteller.Context.DSL.RenderingSpec" already uses.
describeMessage :: LLM.Message m -> (LLM.MessageDirection, Text)
describeMessage msg@(LLM.UserText t)      = (LLM.messageDirection msg, t)
describeMessage msg@(LLM.AssistantText t) = (LLM.messageDirection msg, t)
describeMessage msg                       = (LLM.messageDirection msg, "<unsupported in this test>")

resolveOverrideDefinitionSpec :: Spec
resolveOverrideDefinitionSpec = describe "resolveOverrideDefinition" $ do
  it "returns Nothing when there's no override" $
    resolveOverrideDefinition Nothing `shouldBe` Nothing

  it "returns Nothing on a malformed override" $
    resolveOverrideDefinition (Just "as \"unterminated:") `shouldBe` Nothing

  it "returns Just the parsed Definition, at whatever arity it parses to, when it parses" $
    case resolveOverrideDefinition (Just "charname:\n  charname\n") of
      Just _  -> pure ()
      Nothing -> expectationFailure "expected a valid override to parse"

-- | 'resolveContext0' end to end, against both real interpreters, using
--   @context.style@ (@read \"style.md\" | orifempty \"\"@, see
--   'Storyteller.Context.DSL.Library.contextStyleDef') -- a real
--   'defaultLibraryOrder' slot, so this exercises the actual production
--   resolution path rather than a caller-supplied Haskell stand-in (see
--   this module's own Haddock on why the old @context.greeting@ test name
--   is gone). Each case is two steps: 'resolveContext0' itself (a plain
--   'Sem' call, already fully run) hands back a 'Value', which still
--   needs a *second*, separate 'runContextValue' call to force its own
--   'valueDefault'.
resolveContext0Spec :: Spec
resolveContext0Spec = describe "resolveContext0" $ do
  it "falls through to the compiled-in default when nothing is staged" $
    run (testStack $ do
      seedBranch "main" []
      runBranchAndFS @Main (BranchName "main") $ do
        v <- resolveContext0 @Main "context.style"
        runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right ""

  it "a staged override is visible to a lookup in the same interpretation" $
    run (testStack $ do
      seedBranch "main" []
      runBranchAndFS @Main (BranchName "main") $ do
        setContextOverride "context.style" "\"staged text\"\n"
        v <- resolveContext0 @Main "context.style"
        runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "staged text"

  -- | An override at the wrong arity for a name some *default* calls is
  --   rejected during compilation, and the default stands.
  --
  --   @context.style@ is called with no arguments by @context.custom@
  --   (the starting template for a user-defined agent, see
  --   'Storyteller.Context.DSL.Library.contextCustomDef'), so a 1-arity
  --   override of it can't compile — and 'buildContextLibrary' drops the
  --   override rather than the default, which is the only choice that
  --   leaves a working library.
  --
  --   This used to assert the opposite (the override landed, and the
  --   *lookup* failed) and was correct when written: nothing inside the
  --   DSL called @context.style@ then, so there was nothing to catch the
  --   mismatch up front. Adding a caller moved it into the class every
  --   other @context.*@ name has always been in. Worth being precise
  --   about, since the two behaviours are easy to conflate: a name with
  --   no DSL-internal caller still accepts a wrong-arity override and
  --   still fails at the call (see 'resolveContext0''s own contract) --
  --   that class just no longer has @context.style@ in it.
  it "a staged override at the wrong arity for a name a default calls is rejected, leaving the default" $
    run (testStack $ do
      seedBranch "main" []
      runBranchAndFS @Main (BranchName "main") $ do
        setContextOverride "context.style" "charname:\n  charname\n"
        v <- resolveContext0 @Main "context.style"
        runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right ""   -- the compiled-in default: `read "style.md" | orifempty ""`

  -- The crash this replaced: a wrong-arity override used to surface as a
  -- pure `error` call from inside 'buildContextLibrary' (reporting the
  -- *default* it broke, which is not something a project can act on),
  -- taking down every request that resolved any context at all, not just
  -- ones touching the overridden name. An unrelated slot has to keep
  -- resolving normally with that same broken override staged.
  it "an unrelated slot still resolves while a wrong-arity override is staged" $
    run (testStack $ do
      seedBranch "main" [("lore/a.md", "lore content")]
      runBranchAndFS @Main (BranchName "main") $ do
        setContextOverride "context.style" "charname:\n  charname\n"
        v <- resolveContext0 @Main "context.lore"
        runContextValue @Main (messagesText <$> valueDefault v))
    `shouldSatisfy` either (const False) (T.isInfixOf "lore content")

  -- A good override committed alongside a bad one must survive: rejection
  -- is per-name, never "give up on this project's overrides".
  it "keeps a valid override while rejecting a wrong-arity one" $
    run (testStack $ do
      seedBranch "main" []
      runBranchAndFS @Main (BranchName "main") $ do
        setContextOverride "context.style" "charname:\n  charname\n"
        setContextOverride "context.lore" "\"my own lore\"\n"
        v <- resolveContext0 @Main "context.lore"
        runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "my own lore"

  it "a staged override takes priority over a same-named branch commit" $
    run (testStack $ do
      seedBranch "main" []
      seedBranch (unBranchName contextsBranchName)
        [("context/style.dsl", "\"from the branch\"\n")]
      runBranchAndFS @Main (BranchName "main") $
        interpretContextStorageFS $ do
          setContextOverride "context.style" "\"staged text\"\n"
          v <- resolveContext0 @Main "context.style"
          runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "staged text"

  it "runs a real committed override, positioned at the caller's own branch, not the Contexts branch" $
    run (testStack $ do
      seedBranch "main" [("greeting.md", "hello from main")]
      seedBranch (unBranchName contextsBranchName)
        [("context/style.dsl", "< read \"greeting.md\"\n")]
      runBranchAndFS @Main (BranchName "main") $
        interpretContextStorageFS $ do
          v <- resolveContext0 @Main "context.style"
          runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "hello from main"

-- | 'resolveContext1' end to end -- the 1-arity counterpart, using
--   @context.other@ (a real 1-arity slot: @path@, then every non-lore\/
--   chapters file except @path@ itself -- see 'contextOtherDef'), for the
--   same reason 'resolveContext0Spec' moved off its own test-local stand-in.
resolveContext1Spec :: Spec
resolveContext1Spec = describe "resolveContext1" $ do
  it "falls back to the compiled-in default when no override is committed" $
    run (testStack $ do
      seedBranch "main" []
      runBranchAndFS @Main (BranchName "main") $ do
        v <- resolveContext1 @Main "context.other" "target.md"
        runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "## Other notes"

  it "resolves and runs a real 1-arity override too, with the real argument reaching it" $
    run (testStack $ do
      seedBranch "main" []
      seedBranch (unBranchName contextsBranchName)
        [("context/other.dsl", "name:\n  \"overridden for %name%\"\n")]
      runBranchAndFS @Main (BranchName "main") $
        interpretContextStorageFS $ do
          v <- resolveContext1 @Main "context.other" "target.md"
          runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "overridden for target.md"

-- | 'resolveAdhoc0' -- what a per-call @pinnedPrograms@ entry
--   ('Server.Writer.File.chatWriter''s own wire field) resolves through:
--   no name, no compiled-in default to fall back to, just "run this
--   0-arity program against the library, or fail." A bare call to an
--   existing library name (@rules.magic@, say -- any project's own
--   committed @context.*@-style definition) resolves the same way any
--   other cross-definition reference does, since it's compiled against
--   the exact same 'buildContextLibrary' table 'resolveContext0'\/
--   'resolveContext1' use.
resolveAdhoc0Spec :: Spec
resolveAdhoc0Spec = describe "resolveAdhoc0" $ do
  it "runs a bare literal 0-arity program directly" $
    run (testStack $ do
      seedBranch "main" []
      runBranchAndFS @Main (BranchName "main") $ do
        v <- resolveAdhoc0 @Main "\"hand-authored pinned text\"\n"
        runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "hand-authored pinned text"

  it "resolves a bare call to a project's own committed library definition" $
    run (testStack $ do
      seedBranch "main" [("sheet.md", "# Aria\n\nA wandering rogue.")]
      seedBranch (unBranchName contextsBranchName)
        [("rules/magic.dsl", "\"the rules of magic\"\n")]
      runBranchAndFS @Main (BranchName "main") $
        interpretContextStorageFS $ do
          v <- resolveAdhoc0 @Main "rules.magic\n"
          runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "the rules of magic"

  it "fails (rather than silently contributing nothing) for a 1-arity program -- there is no slot default to fall back to" $
    (run (testStack $ do
      seedBranch "main" []
      runBranchAndFS @Main (BranchName "main") $ do
        v <- resolveAdhoc0 @Main "name:\n  \"got %name%\"\n"
        runContextValue @Main (messagesText <$> valueDefault v))
      :: Either String Text)
    `shouldSatisfy` \case
      Left _  -> True
      Right _ -> False

  -- The frontend's checkbox-generated lore-override shape (dslCompose.ts's
  -- `renderLoreProgram`): a banner plus one `loreEntry [path]` call per
  -- chosen file -- calling the real library function directly (not
  -- reproducing its body) so the per-file `## <path>` heading can never
  -- drift from what the real default (`contextLoreDef`) itself produces.
  -- `[path]` (Parser.hs's bracket-glob literal), not a bare token: the
  -- generator needs to handle any real filename, including ones with
  -- spaces, uniformly -- see the second example below.
  it "a loreEntry [path] call per chosen path reproduces the real per-file heading+content shape" $
    run (testStack $ do
      seedBranch "main"
        [ ("lore/a.md", "note about a")
        , ("lore/b.md", "note about b")
        ]
      runBranchAndFS @Main (BranchName "main") $
        interpretContextStorageFS $ do
          v <- resolveAdhoc0 @Main frontendCheckboxProgram
          runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "## Story background\n## lore/a.md\nnote about a\n## lore/b.md\nnote about b"

  it "the same shape works for a path containing spaces, which a bare token could never tokenize as one argument" $
    run (testStack $ do
      seedBranch "main" [("lore/file with spaces.md", "note in a spaced file")]
      runBranchAndFS @Main (BranchName "main") $
        interpretContextStorageFS $ do
          v <- resolveAdhoc0 @Main "\"## Story background\"\nloreEntry [lore/file with spaces.md]\n"
          runContextValue @Main (messagesText <$> valueDefault v))
    `shouldBe` Right "## Story background\n## lore/file with spaces.md\nnote in a spaced file"
  where
    -- Exactly `renderLoreProgram`'s own output shape (dslCompose.ts) for
    -- two chosen paths.
    frontendCheckboxProgram = T.unlines
      [ "\"## Story background\""
      , "loreEntry [lore/a.md]"
      , "loreEntry [lore/b.md]"
      ]

-- | 'buildContextLibrary' end to end -- the headline behavior of the
--   compile-the-whole-sequence-and-reject redesign, not yet exercised
--   directly by anything above (those tests all go through
--   'resolveContext0'\/'resolveContext1', which only ever surface a
--   rejection indirectly, as "the default ran instead").
buildContextLibrarySpec :: Spec
buildContextLibrarySpec = describe "buildContextLibrary" $ do
  it "rejects an override whose own body references an unresolvable name, reporting it, and the default still runs" $
    run (testStack $ do
      seedBranch "main" []
      runBranchAndFS @Main (BranchName "main") $ do
        setContextOverride "context.style" "\"prefix: %this.does.not.exist%\"\n"
        rejected <- rejectedOverrides @Main
        v <- resolveContext0 @Main "context.style"
        text <- runContextValue @Main (messagesText <$> valueDefault v)
        pure (rejected, text))
    `shouldBe` Right (["context.style"], "")

  it "accepts an override with no bad references, reporting nothing rejected" $
    run (testStack $ do
      seedBranch "main" []
      runBranchAndFS @Main (BranchName "main") $ do
        setContextOverride "context.style" "\"a fine override\"\n"
        rejectedOverrides @Main)
    `shouldBe` Right []

  -- | An override interpolating its own name (@%context.style%@, not just
  --   a bare 'EIdent' reference -- the same resolution rule, just reached
  --   through string interpolation instead) resolves to the compiled
  --   default sitting one slot behind it in the sequence, not itself and
  --   not a rejection -- see 'Storyteller.Core.Context.spliceOverrides''s
  --   own Haddock on why a default-then-override pair at one name is what
  --   makes this work, and 'Storyteller.Context.DSL.Compile.definitionBinding''s
  --   Haddock for why @%name%@ spans are checked the identical way a bare
  --   'EIdent' already is.
  it "an override interpolating its own name resolves to the compiled default, not itself" $
    run (testStack $ do
      seedBranch "main" [("style.md", "write in past tense")]
      runBranchAndFS @Main (BranchName "main") $ do
        setContextOverride "context.style" "\"prefix: %context.style%\"\n"
        rejected <- rejectedOverrides @Main
        v <- resolveContext0 @Main "context.style"
        text <- runContextValue @Main (messagesText <$> valueDefault v)
        pure (rejected, text))
    `shouldBe` Right ([], "prefix: write in past tense")

-- | Just the rejected-name half of 'buildContextLibrary''s result, forced
--   inside a real 'runContextValue' call -- 'buildContextLibrary' needs
--   'branch'\'s full content-effects row resolved to build its dictionaries
--   at all (see its own Haddock), which a bare 'Map Name Text -> (Library
--   r, [Name])' call sitting outside that interpreted row can't supply on
--   its own; the accepted 'Library' half is still thrown away unused, only
--   the effect row itself needs to be concrete.
rejectedOverrides
  :: forall branch r. Members '[BranchOp branch, Branches, BranchResolve, ContextStorage, Fail] r
  => Sem r [Name]
rejectedOverrides = do
  overrides <- getContextOverrides
  let (_lib, rejected) = buildContextLibrary @branch overrides
        :: (Library (ContextRow branch r), [Name])
  runContextValue @branch (pure rejected)
