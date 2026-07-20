{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
--   at all -- its whole interpreter runs in @'Core.StoreM' m => 'StoreT'
--   m@, and the one operation that can't be (resolving a 'BranchName' to
--   a commit) is a plain injected function ('BranchResolver'). This
--   module is exactly where that injection happens for real:
--   'resolveBranch' below, closing over 'getBranch', is the *only*
--   Polysemy-specific code anywhere in this file's use of the DSL --
--   everything else (compiling, running, forcing) is the same generic
--   'StoreT' pipeline regardless of backend.
module Storyteller.Context.DSL.CompileSpec (spec) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail)

import Runix.Git (Git)

import qualified Storage.Core as Core
import Storage.Core (StoreT)

import qualified Storage.Ops as Ops
import Storyteller.Core.Git (runBranchOpGit, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (Branch(..), BranchName(..), TickId(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Context.DSL.AST (Definition, Name)
import Storyteller.Context.DSL.Compile
import Storyteller.Context.DSL.Parser (parseDefinition, renderParseErr)
import Storyteller.Context.DSL.Value

type DSL r = Members '[Git, StoryStorage, Fail] r

-- | The one Polysemy-specific piece: a real 'BranchResolver', closing
--   over 'getBranch'. Everything in "Storyteller.Context.DSL.Compile"
--   downstream of this is generic 'StoreT' code that has no idea this
--   is how branches get resolved.
resolveBranch :: DSL r => BranchResolver (Sem r)
resolveBranch name = getBranch name >>= \case
  Nothing -> pure Nothing
  Just b  -> pure (Just (Core.ObjectHash (unTickId (branchHead b))))

-- | Resolves @bname@ once and runs one whole 'StoreT' computation
--   seeded at its head -- the one 'Core.runStoreT' dispatch a real host
--   would also do, just wrapped for test convenience. @k@ gets the
--   resolved commit too, since most callers immediately need it again
--   for 'treeValueOfCommit'.
withBranchCommit :: DSL r => Text -> (Core.ObjectHash -> StoreT (Sem r) a) -> Sem r a
withBranchCommit bname k = do
  mCommit <- resolveBranch (BranchName bname)
  commit  <- maybe (fail ("branch not found: " <> T.unpack bname)) pure mCommit
  fst <$> Core.runStoreT commit (k commit)

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

-- | Parses source, generic over any 'MonadFail' -- usable both at the
--   'Sem' level (test setup) and from inside a 'StoreT' computation
--   (both 'Core.StoreM'\'s constraint bundle and plain 'Sem' satisfy
--   'MonadFail').
parseOrFail :: MonadFail m => Text -> m Definition
parseOrFail src = case parseDefinition "<test>" src of
  Left err  -> fail (T.unpack (renderParseErr err))
  Right def -> pure def

-- | Compiles @src@ against @bname@'s current tree, applies it to
--   @args@ (each a plain-text leaf), and forces both the result's own
--   default text and one level of its named exports' text -- enough to
--   assert against without dragging 'Value''s laziness machinery into
--   every test.
runDsl :: Text -> Text -> [Text] -> Sem (StoryStorage : TestEffects '[]) (Text, Map Name Text)
runDsl bname src args = do
  def <- parseOrFail src
  withBranchCommit bname $ \commit -> do
    scope <- treeValueOfCommit commit
    let argActions = map (pure . leafValue . (: []) . User) args
    v         <- compileDefinition coreFilters resolveBranch def scope argActions
    defMsgs   <- valueDefault v
    entryText <- mapM (\act -> messagesText <$> (valueDefault =<< act)) (valueEntries v)
    pure (messagesText defMsgs, entryText)

runCase :: Text -> [(FilePath, Text)] -> Text -> [Text] -> Either String (Text, Map Name Text)
runCase bname files src args = run $ testStack $ do
  seedBranch bname files
  runDsl bname src args

spec :: Spec
spec = do
  injuryExampleSpec
  absenceSpec
  crossBranchSpec
  forLoopSpec
  forLoopEntriesSpec
  forceUserRoleSpec

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
runInjuryCase :: Text -> [(FilePath, Text)] -> Either String Text
runInjuryCase bname files = run $ testStack $ do
  seedBranch bname files
  withBranchCommit bname $ \commit -> do
    scope     <- treeValueOfCommit commit
    ctxDef    <- parseOrFail "as \"injury\": read status/injury.md\n"
    ctxValue  <- compileDefinition coreFilters resolveBranch ctxDef scope []
    callerDef <- parseOrFail "ctx:\n  in ctx: read \"injury\" | orifempty \"not injured\"\n"
    result    <- compileDefinition coreFilters resolveBranch callerDef scope [pure ctxValue]
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

crossBranchSpec :: Spec
crossBranchSpec = describe "in (charname | branch): ... (cross-branch read)" $
  it "reads a named character's own branch, not the calling branch" $
    let src = "charname:\n\
              \  in (charname | branch): read \"sheet.md\"\n"
    in run (testStack $ do
         seedBranch "main" []
         seedBranch "character/aria" [("sheet.md", "Aria is a wandering rogue.")]
         runDsl "main" src ["aria"])
       `shouldBe` Right ("Aria is a wandering rogue.", Map.empty)

forLoopSpec :: Spec
forLoopSpec = describe "for/as over a glob (Chekhov's-gun list example)" $
  it "exports one named entry per matched file, each holding that file's own content" $
    runCase "main"
      [ ("tracking/gun.md", "a gun on the mantelpiece")
      , ("tracking/letter.md", "an unopened letter")
      , ("other/unrelated.md", "should not be matched")
      ]
      "as \"open\":\n\
      \  for f in tracking/**.md:\n\
      \    as f: read f\n"
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
      withBranchCommit "main" $ \commit -> do
        scope <- treeValueOfCommit commit
        def   <- parseOrFail
          "as \"open\":\n\
          \  for f in tracking/**.md:\n\
          \    as f: read f\n"
        v     <- compileDefinition coreFilters resolveBranch def scope []
        Just openAction <- pure (Map.lookup "open" (valueEntries v))
        openVal <- openAction
        mapM (\act -> messagesText <$> (valueDefault =<< act)) (valueEntries openVal))
    `shouldBe` Right (Map.fromList
      [ ("tracking/gun.md", "a gun on the mantelpiece")
      , ("tracking/letter.md", "an unopened letter")
      ])

-- | @< read file@ -- a 'read' would otherwise produce a role-undecided
--   'FileRead'; @<@ forces it to read as ordinary authored text instead.
forceUserRoleSpec :: Spec
forceUserRoleSpec = describe "< <expr> (force User role)" $
  it "re-tags a read's FileRead messages as User, leaving the text itself unchanged" $
    run (testStack $ do
      seedBranch "main" [("notes.md", "the door was left ajar")]
      withBranchCommit "main" $ \commit -> do
        scope <- treeValueOfCommit commit
        def   <- parseOrFail "< read notes.md\n"
        v     <- compileDefinition coreFilters resolveBranch def scope []
        valueDefault v)
    `shouldBe` Right [User "the door was left ajar"]
