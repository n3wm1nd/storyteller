{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | End-to-end: parse a definition from @CONTEXT-DSL.md@-shaped source,
--   compile it with 'Storyteller.Context.DSL.Compile.compileDefinition'
--   against a real (mock-git-backed) branch, and check the resulting
--   'Value''s forced text -- the thing "Storyteller.Context.DSL.ParserSpec"
--   can't check on its own (it only ever inspects the AST, never runs
--   it).
--
--   "Storyteller.Context.DSL.Compile" itself has no Polysemy dependency
--   and no type parameter anywhere -- 'Value' is monomorphic, and every
--   deferred computation is an 'Action', generic over any backend that
--   can satisfy 'Core.StoreM'. The one thing that can't be expressed
--   that way (resolving a 'BranchName' to a commit) is threaded through
--   'runAction' as an explicit parameter rather than baked into any one
--   'Action'. This module is exactly where a *real* one gets supplied:
--   'resolveBranch' below, closing over 'getBranch', is the *only*
--   Polysemy-specific code anywhere in this file's use of the DSL.
module Storyteller.Context.DSL.CompileSpec (spec) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail)

import Runix.Git (Git)

import qualified Storage.Core as Core

import qualified Storage.Ops as Ops
import Storyteller.Core.Git (runBranchOpGit, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (Branch(..), BranchName(..), TickId(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Context.DSL.AST (Definition, Name)
import Storyteller.Context.DSL.Compile
import Storyteller.Context.DSL.QQ (dsl)
import Storyteller.Context.DSL.Value

type DSL r = Members '[Git, StoryStorage, Fail] r

-- | The one Polysemy-specific piece: a real 'BranchResolver', closing
--   over 'getBranch'. Everything else in this file is 'Action' code
--   that has no idea this is how branches get resolved -- it only ever
--   sees a resolver at the point 'runAction' finally runs one.
resolveBranch :: DSL r => BranchResolver (Sem r)
resolveBranch name = getBranch name >>= \case
  Nothing -> pure Nothing
  Just b  -> pure (Just (Core.ObjectHash (unTickId (branchHead b))))

-- | Creates a branch and seeds it with files, all in one short-lived
--   'BranchOp' scope -- setup only, the DSL interpreter itself never
--   touches this. Deliberately commits each file as a real 'Ops.addAtom'
--   tick (via 'runStorage', not 'Runix.FileSystem.writeFile') -- plain
--   'writeFile' only ever lands in the *ambient* working tree (see
--   "Storage.Core"'s own "two entirely independent pieces of state"
--   split), never in the committed chain the DSL's Reader scope reads
--   off, so test fixtures need to actually commit, the same way real
--   story edits do.
seedBranch :: Text -> [(FilePath, Text)] -> Sem (StoryStorage : TestEffects '[]) ()
seedBranch name files = do
  _ <- createBranch (BranchName name)
  runBranchOpGit @Main (BranchName name)
    (mapM_ (\(path, content) -> runStorage @Main (Ops.addAtom path content)) files)

-- | Compiles @def@ against @bname@'s current tree, applies it to
--   @args@ (each a plain-text leaf), and forces both the result's own
--   default text and one level of its named exports' text -- enough to
--   assert against without dragging 'Value''s laziness machinery into
--   every test. The whole DSL side is one 'Action', run once via
--   'resolveBranch'.
runDsl :: Text -> Definition -> [Text] -> Sem (StoryStorage : TestEffects '[]) (Text, Map Name Text)
runDsl bname def args = runAction go resolveBranch
  where
    go = do
      let argActions = map (pure . leafValue . (: []) . User) args
      v         <- runDefinitionOnBranch (BranchName bname) def argActions
      defMsgs   <- valueDefault v
      entryText <- mapM (\act -> messagesText <$> (valueDefault =<< act)) (valueEntries v)
      pure (messagesText defMsgs, entryText)

runCase :: Text -> [(FilePath, Text)] -> Definition -> [Text] -> Either String (Text, Map Name Text)
runCase bname files def args = run $ testStack $ do
  seedBranch bname files
  runDsl bname def args

spec :: Spec
spec = do
  injuryExampleSpec
  absenceSpec
  crossBranchSpec
  forLoopSpec
  forLoopEntriesSpec
  forceUserRoleSpec
  localFunctionInForLoopSpec

-- | Demonstrates the doc's own follow-up sentence ("The raw fact stays
--   reachable via @in thisResult: read \"injury\"@") properly. A
--   definition never names itself -- its only name is the file it lives
--   at (see "Function definitions" in the spec: "identified entirely by
--   its path... no separate function\/def keyword") -- so "thisResult"
--   only makes sense from a *caller* that already has the Value in
--   hand. That's the "Builtins are not filters" convention: an
--   already-computed 'Value' passed in as an ordinary parameter, not a
--   made-up self-reference. A real caller would resolve the injury
--   context by its path on a @Contexts@ branch (not implemented yet --
--   see "Storyteller.Context.DSL.Compile"'s module haddock); compiling
--   it directly here and passing the result in stands in for that.
ctxDef :: Definition
ctxDef = [dsl|
as "injury": read status/injury.md
|]

callerDef :: Definition
callerDef = [dsl|
ctx:
  in ctx: read "injury" | orifempty "not injured"
|]

runInjuryCase :: Text -> [(FilePath, Text)] -> Either String Text
runInjuryCase bname files = run $ testStack $ do
  seedBranch bname files
  runAction go resolveBranch
  where
    go = do
      scope    <- treeValueOfBranch (BranchName bname)
      ctxValue <- compileDefinition coreFilters ctxDef scope []
      result   <- compileDefinition coreFilters callerDef scope [pure ctxValue]
      messagesText <$> valueDefault result

injuryExampleSpec :: Spec
injuryExampleSpec = describe "injury/status continuity example" $
  it "a caller reaches a named export's raw value via `in`, passed in as an ordinary parameter" $
    runInjuryCase "main" [("status/injury.md", "a sprained ankle")]
      `shouldBe` Right "a sprained ankle"

absenceSpec :: Spec
absenceSpec = describe "absence, not an error (Non-goals)" $
  it "falls back through orifempty when the tracked file doesn't exist" $
    runInjuryCase "main" []
      `shouldBe` Right "not injured"

crossBranchDef :: Definition
crossBranchDef = [dsl|
charname:
  in (charname | branch): read "sheet.md"
|]

crossBranchSpec :: Spec
crossBranchSpec = describe "in (charname | branch): ... (cross-branch read)" $
  it "reads a named character's own branch, not the calling branch" $
    run (testStack $ do
         seedBranch "main" []
         seedBranch "character/aria" [("sheet.md", "Aria is a wandering rogue.")]
         runDsl "main" crossBranchDef ["aria"])
       `shouldBe` Right ("Aria is a wandering rogue.", Map.empty)

-- | Shared by 'forLoopSpec' (checks the container's own default text)
--   and 'forLoopEntriesSpec' (checks what's inside each entry) -- both
--   exercise the same definition.
openTrackingDef :: Definition
openTrackingDef = [dsl|
as "open":
  for f in tracking/**.md:
    as f: read f
|]

forLoopSpec :: Spec
forLoopSpec = describe "for/as over a glob (Chekhov's-gun list example)" $
  it "exports one named entry per matched file, each holding that file's own content" $
    runCase "main"
      [ ("tracking/gun.md", "a gun on the mantelpiece")
      , ("tracking/letter.md", "an unopened letter")
      , ("other/unrelated.md", "should not be matched")
      ]
      openTrackingDef
      []
      `shouldBe` Right ("", Map.fromList [("open", "")])
      -- 'open' itself has no default text (it's a pure container of
      -- named exports) -- see the follow-up test below for what's
      -- actually inside it.

forLoopEntriesSpec :: Spec
forLoopEntriesSpec = describe "for/as nested entries" $
  it "each matched path's own entry holds that file's content" $
    run (testStack $ do
      seedBranch "main"
        [ ("tracking/gun.md", "a gun on the mantelpiece")
        , ("tracking/letter.md", "an unopened letter")
        ]
      runAction go resolveBranch)
    `shouldBe` Right (Map.fromList
      [ ("tracking/gun.md", "a gun on the mantelpiece")
      , ("tracking/letter.md", "an unopened letter")
      ])
  where
    go = do
      scope   <- treeValueOfBranch (BranchName "main")
      v       <- compileDefinition coreFilters openTrackingDef scope []
      Just openAction <- pure (Map.lookup "open" (valueEntries v))
      openVal <- openAction
      mapM (\act -> messagesText <$> (valueDefault =<< act)) (valueEntries openVal)

-- | @< read file@ -- a 'read' would otherwise produce a role-undecided
--   'FileRead'; @<@ forces it to read as ordinary authored text instead.
forceUserRoleDef :: Definition
forceUserRoleDef = [dsl|
< read notes.md
|]

forceUserRoleSpec :: Spec
forceUserRoleSpec = describe "< <expr> (force User role)" $
  it "re-tags a read's FileRead messages as User, leaving the text itself unchanged" $
    run (testStack $ do
      seedBranch "main" [("notes.md", "the door was left ajar")]
      runAction go resolveBranch)
    `shouldBe` Right [User "the door was left ajar"]
  where
    go = do
      scope <- treeValueOfBranch (BranchName "main")
      v     <- compileDefinition coreFilters forceUserRoleDef scope []
      valueDefault v

-- | A local function isn't a different kind of thing from a plain value
--   -- it's bound fresh every iteration exactly like any other @let@
--   (rule 4), and calling it composes with the loop variable exactly
--   like calling any named context would. Guards against a binding
--   mechanism that (accidentally) special-cased top-level 'SLet' and
--   left 'BFun' bindings inside a nested block, like a @for@ body,
--   unable to actually be called -- and checks the *content*, not just
--   the shape, so a function silently ignoring its argument (always
--   resolving the same loop iteration) wouldn't slip through.
localFunctionInForLoopDef :: Definition
localFunctionInForLoopDef = [dsl|
as "results":
  for f in tracking/**.md:
    wrap = x: x | filewithname
    as f: wrap f
|]

localFunctionInForLoopSpec :: Spec
localFunctionInForLoopSpec = describe "a local function bound fresh each for-loop iteration" $
  it "is callable from within the same iteration, using that iteration's own loop variable" $
    run (testStack $ do
      seedBranch "main"
        [ ("tracking/gun.md", "a gun on the mantelpiece")
        , ("tracking/letter.md", "an unopened letter")
        ]
      runAction go resolveBranch)
    `shouldBe` Right (Map.fromList [("tracking/gun.md", "gun"), ("tracking/letter.md", "letter")])
  where
    go = do
      scope   <- treeValueOfBranch (BranchName "main")
      v       <- compileDefinition coreFilters localFunctionInForLoopDef scope []
      Just resultsAction <- pure (Map.lookup "results" (valueEntries v))
      results <- resultsAction
      mapM (\act -> messagesText <$> (valueDefault =<< act)) (valueEntries results)
