{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | End-to-end: parse a definition from @CONTEXT-DSL.md@-shaped source
--   via @['dsl'| ... |]@, run the curried function it splices to
--   (see "Storyteller.Context.DSL.QQ") against a real (mock-git-backed)
--   branch, and check the resulting 'Value''s forced text -- the thing
--   "Storyteller.Context.DSL.ParserSpec" can't check on its own (it only
--   ever inspects the AST, never runs it).
--
--   "Storyteller.Context.DSL.Compile" itself has no Polysemy dependency
--   and no type parameter anywhere -- 'Value' is monomorphic, and every
--   deferred computation is an 'Action', generic over any backend that
--   can satisfy 'Core.StoreM' and 'MonadBranch'. The one thing that can't
--   be expressed via 'Core.StoreM' alone (resolving a 'BranchName' to a
--   commit) is exactly 'MonadBranch'. This module is exactly where a
--   *real* instance gets supplied: the @instance MonadBranch (Sem r)@
--   below, closing over 'getBranch', is the *only* Polysemy-specific
--   code anywhere in this file's use of the DSL.
module Storyteller.Context.DSL.CompileSpec (spec) where

import Control.Monad (void)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
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

import Storyteller.Core.Context (buildContextLibrary)

import Storyteller.Context.DSL.AST (Name)
import Storyteller.Context.DSL.Compile (Binding, bval, fn1, journalDelta)
import qualified Storyteller.Context.DSL.Library as CtxLibrary
import Storyteller.Context.DSL.QQ (dsl)
import Storyteller.Context.DSL.Value
import Storyteller.Writer.Presence (recordPresence)
import Storyteller.Writer.Types (Character(..), PresenceEvent(..))

-- | The real, production 'MonadBranch' instance now lives in
--   "Storyteller.Core.Git" (closing over 'getBranch', same as this file's
--   own removed orphan used to) -- everything else here is 'Action' code
--   that has no idea this is how branches get resolved, it only ever
--   reaches for it via 'askBranch'. Creates a branch and seeds it with files, all in one short-lived
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

-- | Resolves @bname@ to its current head and runs @act@'s 'StoreT'
--   computation from there -- the ambient position every
--   'Storyteller.Context.DSL.Compile.currentScope'-derived call in this
--   file relies on, established here rather than threaded through the
--   DSL's own API.
runDslOn :: BranchName -> Action a -> Sem (StoryStorage : TestEffects '[]) a
runDslOn bname act = resolveBranch bname >>= \case
  Nothing -> fail ("branch not found: " <> T.unpack (unBranchName bname))
  Just h  -> fst <$> Core.runStoreT h (runAction act (buildContextLibrary Map.empty))

-- | A plain-text leaf, ready to apply to a @['dsl'| ... |]@-spliced
--   function's own parameters -- 'bval' baked in, so passing a leaf
--   value stays exactly as terse as it was before 'Binding' became the
--   parameter currency (see 'Storyteller.Context.DSL.Compile.Binding'\'s
--   own haddock).
textArg :: Text -> Binding
textArg = bval . pure . leafValue . (: []) . User

spec :: Spec
spec = do
  injuryExampleSpec
  absenceSpec
  crossBranchSpec
  forLoopSpec
  forLoopEntriesSpec
  forceUserRoleSpec
  localFunctionInForLoopSpec
  hostFunctionParamSpec
  excludeFilterSpec
  sortByFilterSpec
  sortByThenReexportSpec
  excludeByAnotherDefinitionSpec
  assistantWrapsExprSpec
  nameAbstractFilterSpec
  journalDeltaSpec
  contextCharacterSpec
  forOverBindingResultSpec

-- | Demonstrates the doc's own follow-up sentence ("The raw fact stays
--   reachable via @in thisResult: read \"injury\"@") properly. A
--   definition never names itself -- its only name is the file it lives
--   at (see "Function definitions" in the spec: "identified entirely by
--   its path... no separate function\/def keyword") -- so "thisResult"
--   only makes sense from a *caller* that already has the Value in
--   hand. That's the "Builtins are not filters" convention: an
--   already-computed 'Action' 'Value' passed in as an ordinary
--   parameter, not a made-up self-reference. A real caller would
--   resolve the injury context by its path on a @Contexts@ branch (not
--   implemented yet -- see "Storyteller.Context.DSL.Compile"'s module
--   haddock); compiling it directly here and passing the result in
--   stands in for that.
ctxDsl :: Action Value
ctxDsl = [dsl|
as "injury": read status/injury.md
|]

callerDsl :: Binding -> Action Value
callerDsl = [dsl|
ctx:
  in ctx: read "injury" | orifempty "not injured"
|]

runInjuryCase :: Text -> [(FilePath, Text)] -> Either String Text
runInjuryCase bname files = run $ testStack $ do
  seedBranch bname files
  runDslOn (BranchName bname) (messagesText <$> (valueDefault =<< callerDsl (bval ctxDsl)))

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

crossBranchDsl :: Binding -> Action Value
crossBranchDsl = [dsl|
charname:
  in (charname | branch): read "sheet.md"
|]

crossBranchSpec :: Spec
crossBranchSpec = describe "in (charname | branch): ... (cross-branch read)" $
  it "reads a named character's own branch, not the calling branch" $
    run (testStack $ do
         seedBranch "main" []
         seedBranch "character/aria" [("sheet.md", "Aria is a wandering rogue.")]
         runDslOn (BranchName "main")
           (messagesText <$> (valueDefault =<< crossBranchDsl (textArg "aria"))))
       `shouldBe` Right "Aria is a wandering rogue."

-- | Shared by 'forLoopSpec' (checks the container's own default text)
--   and 'forLoopEntriesSpec' (checks what's inside each entry) -- both
--   exercise the same definition.
openTrackingDsl :: Action Value
openTrackingDsl = [dsl|
as "open":
  for f in tracking/**.md:
    as f: read f
|]

forLoopSpec :: Spec
forLoopSpec = describe "for/as over a glob (Chekhov's-gun list example)" $
  it "exports one named entry per matched file, each holding that file's own content" $
    run (testStack $ do
      seedBranch "main"
        [ ("tracking/gun.md", "a gun on the mantelpiece")
        , ("tracking/letter.md", "an unopened letter")
        , ("other/unrelated.md", "should not be matched")
        ]
      runDslOn (BranchName "main") go)
    `shouldBe` Right ("", Map.fromList [("open", "")])
      -- 'open' itself has no default text (it's a pure container of
      -- named exports) -- see the follow-up test below for what's
      -- actually inside it.
  where
    go = do
      v         <- openTrackingDsl
      defMsgs   <- valueDefault v
      entryText <- mapM (\(k, act) -> (,) k . messagesText <$> (valueDefault =<< act)) (valueEntries v)
      pure (messagesText defMsgs, Map.fromList entryText :: Map Name Text)

forLoopEntriesSpec :: Spec
forLoopEntriesSpec = describe "for/as nested entries" $
  it "each matched path's own entry holds that file's content" $
    run (testStack $ do
      seedBranch "main"
        [ ("tracking/gun.md", "a gun on the mantelpiece")
        , ("tracking/letter.md", "an unopened letter")
        ]
      runDslOn (BranchName "main") go)
    `shouldBe` Right (Map.fromList
      [ ("tracking/gun.md", "a gun on the mantelpiece")
      , ("tracking/letter.md", "an unopened letter")
      ])
  where
    go = do
      v       <- openTrackingDsl
      Just openAction <- pure (lookup "open" (valueEntries v))
      openVal <- openAction
      entryText <- mapM (\(k, act) -> (,) k . messagesText <$> (valueDefault =<< act)) (valueEntries openVal)
      pure (Map.fromList entryText)

-- | The case that actually motivated widening @for@'s source from a
--   literal 'Storyteller.Context.DSL.AST.PathLit' to a general
--   'Storyteller.Context.DSL.AST.Expr' (see
--   'Storyteller.Context.DSL.AST.SFor''s own haddock): @charactersin path@
--   is an ordinary host 'Binding' call, not a glob match against the
--   Reader-scope tree at all -- its entries come from presence ticks, not
--   file paths. Proving @for@ can iterate it directly, with zero special
--   casing beyond what a filtered glob already needed, is the actual
--   point; 'excludeFilterSpec'\/'sortByFilterSpec' already cover the
--   "iterate a filtered expression" half of the same generalization.
forOverBindingResultSpec :: Spec
forOverBindingResultSpec = describe "for iterates a Binding call's result directly (charactersin), not just a glob" $
  it "iterates the active character identifiers from presence ticks, not any file path" $
    run (testStack $ do
      seedBranch "main" [("scene.md", "")]
      _ <- createBranch (BranchName "character/aria")
      runBranchOpGit @Main (BranchName "main") $
        void (recordPresence @Main "scene.md" (Character (BranchName "character/aria")) Enter)
      runDslOn (BranchName "main") go)
    `shouldBe` Right ["aria"]
  where
    forCharsDsl :: Action Value
    forCharsDsl = [dsl|
for c in (charactersin "scene.md"):
  as c: c
|]
    go = do
      v <- forCharsDsl
      pure (map fst (valueEntries v))

-- | @< read file@ -- a 'read' would otherwise produce a role-undecided
--   'FileRead'; @<@ forces it to read as ordinary authored text instead.
forceUserRoleDsl :: Action Value
forceUserRoleDsl = [dsl|
< read notes.md
|]

forceUserRoleSpec :: Spec
forceUserRoleSpec = describe "< <expr> (force User role)" $
  it "re-tags a read's FileRead messages as User, leaving the text itself unchanged" $
    run (testStack $ do
      seedBranch "main" [("notes.md", "the door was left ajar")]
      runDslOn (BranchName "main") (valueDefault =<< forceUserRoleDsl))
    `shouldBe` Right [User "the door was left ajar"]

-- | A local function isn't a different kind of thing from a plain value
--   -- it's bound fresh every iteration exactly like any other @let@
--   (rule 4), and calling it composes with the loop variable exactly
--   like calling any named context would. Guards against a binding
--   mechanism that (accidentally) special-cased top-level 'SLet' and
--   left 'BFun' bindings inside a nested block, like a @for@ body,
--   unable to actually be called -- and checks the *content*, not just
--   the shape, so a function silently ignoring its argument (always
--   resolving the same loop iteration) wouldn't slip through.
localFunctionInForLoopDsl :: Action Value
localFunctionInForLoopDsl = [dsl|
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
      runDslOn (BranchName "main") go)
    `shouldBe` Right (Map.fromList [("tracking/gun.md", "gun"), ("tracking/letter.md", "letter")])
  where
    go = do
      v       <- localFunctionInForLoopDsl
      Just resultsAction <- pure (lookup "results" (valueEntries v))
      results <- resultsAction
      entryText <- mapM (\(k, act) -> (,) k . messagesText <$> (valueDefault =<< act)) (valueEntries results)
      pure (Map.fromList entryText)

-- | A parameter doesn't have to be a leaf value -- 'Binding' being the
--   actual currency (see its own haddock) means a host can pass in a
--   real Haskell function for "operations that only make sense for this
--   particular call" (the invented-calendar example's own framing for
--   @dateMath@, which this mirrors: parse-tested in "ParserSpec", never
--   compile-tested until now). 'shout' here stands in for that: a pure
--   transform over its own argument, with no reason to see the caller's
--   ambient scope, wrapped via 'fn1' rather than a leaf 'bval'.
shout :: Action Value -> Action Value
shout av = do
  v    <- av
  msgs <- valueDefault v
  pure (leafValue [User (T.toUpper (messagesText msgs))])

hostFunctionDsl :: Binding -> Action Value
hostFunctionDsl = [dsl|
transform:
  transform (read notes.md)
|]

hostFunctionParamSpec :: Spec
hostFunctionParamSpec = describe "a parameter can be a real Haskell function, not just a leaf value" $
  it "applies a host-supplied fn1 the same way it'd apply any other callable" $
    run (testStack $ do
      seedBranch "main" [("notes.md", "quietly written")]
      runDslOn (BranchName "main") (messagesText <$> (valueDefault =<< hostFunctionDsl (fn1 shout))))
    `shouldBe` Right "QUIETLY WRITTEN"

-- | 'without'\/'only' only ever match a full path *exactly* -- 'exclude'
--   is their glob-pattern counterpart, needed for a bucket like the
--   context-selection design's own @"lore"@ scope to drop a whole
--   subtree (@exclude("secrets\/**")@) without enumerating every file in
--   it individually.
excludeFilterDsl :: Action Value
excludeFilterDsl = [dsl|
as "kept":
  in (**/* | exclude("secrets/**")):
    for f in *:
      as f: read f
|]

excludeFilterSpec :: Spec
excludeFilterSpec = describe "expr | exclude(pattern...) (glob-pattern exclusion)" $
  it "drops every entry whose key matches any given glob pattern, keeping the rest" $
    run (testStack $ do
      seedBranch "main"
        [ ("notes.md", "quietly written")
        , ("other.md", "also kept")
        , ("secrets/plan.md", "top secret")
        , ("secrets/sub/deep.md", "very secret")
        ]
      runDslOn (BranchName "main") go)
    `shouldBe` Right (Map.fromList
      [ ("notes.md", "quietly written")
      , ("other.md", "also kept")
      ])
  where
    go = do
      v       <- excludeFilterDsl
      Just keptAction <- pure (lookup "kept" (valueEntries v))
      kept <- keptAction
      entryText <- mapM (\(k, act) -> (,) k . messagesText <$> (valueDefault =<< act)) (valueEntries kept)
      pure (Map.fromList entryText)

-- | @sortBy@ needs no LLM\/content-analysis effect at all -- the ordering
--   ('Storyteller.Writer.Library.naturalKey' on each entry's own key text)
--   is decidable purely from the key set 'Value' already carries, matching
--   what a non-lexical chapter ordering (@ch2@ before @ch11@) needs without
--   forcing a single file's content.
-- | @for@\/glob matching always sorts its own matches lexically (see
--   'Storyteller.Context.DSL.Compile.globMatchPat'), so @ch11.md@ already
--   lands ahead of @ch2.md@ straight out of the loop -- exactly the case
--   'sortBy' exists for. Checked through 'join' (which walks
--   'valueEntries' directly, in whatever order they're already in)
--   rather than a second @for@, since re-globbing would just re-sort
--   lexically and silently undo 'sortBy's own reordering.
sortByFilterDsl :: Action Value
sortByFilterDsl = [dsl|
x =
  for f in *.md:
    as f: f
x | sortBy | join(",")
|]

sortByFilterSpec :: Spec
sortByFilterSpec = describe "expr | sortBy (natural-key reordering)" $
  it "orders entries by naturalKey on their own key text, ch2 before ch11" $
    run (testStack $ do
      seedBranch "main"
        [ ("ch11.md", "eleven")
        , ("ch2.md", "two")
        , ("ch1.md", "one")
        ]
      runDslOn (BranchName "main") (messagesText <$> (valueDefault =<< sortByFilterDsl)))
    `shouldBe` Right "ch1.md,ch2.md,ch11.md"

-- | The realistic shape a stored "chapters" definition actually needs:
--   re-exporting a sorted set of entries through the ordinary @in ...: for
--   f in ...: as f: ...@ idiom, not just observing the order via 'join'.
--   Regression case for 'globMatchPat' no longer force-sorting its own
--   matches lexically (see its own haddock) -- before that fix, this
--   second @for@ would have silently re-alphabetized @x@'s already-sorted
--   entries right back to @ch1, ch11, ch2@.
sortByThenReexportDsl :: Action Value
sortByThenReexportDsl = [dsl|
x =
  for f in *.md:
    as f: f
in (x | sortBy):
  for f in *.md:
    as f: f
|]

sortByThenReexportSpec :: Spec
sortByThenReexportSpec = describe "sortBy's reordering survives a subsequent for/glob re-export" $
  it "keeps natural-key order across an in/for boundary, not just through join" $
    run (testStack $ do
      seedBranch "main"
        [ ("ch11.md", "eleven")
        , ("ch2.md", "two")
        , ("ch1.md", "one")
        ]
      runDslOn (BranchName "main") go)
    `shouldBe` Right ["ch1.md", "ch2.md", "ch11.md"]
  where
    go = do
      v <- sortByThenReexportDsl
      pure (map fst (valueEntries v))

-- | @exclude@ can take another already-computed definition's own result
--   (not just a literal glob pattern) and use its key *names* -- always
--   known purely, no forcing needed -- as the exclusion set. This is the
--   whole point of making @without@\/@only@\/@exclude@ genuinely remove
--   keys rather than just neuter their content: an "everything not
--   already claimed by @lore@" bucket can be built directly from @lore@'s
--   own definition, no pattern duplicated between the two, and the
--   removal survives the second @for@ re-export instead of resurrecting
--   the excluded paths as empty stubs.
loreDsl :: Action Value
loreDsl = [dsl|
for f in lore/**/*:
  as f: read f
|]

excludeByAnotherDefinitionDsl :: Binding -> Action Value
excludeByAnotherDefinitionDsl = [dsl|
lore:
  in (**/* | exclude(lore)):
    for f in **/*:
      as f: read f
|]

excludeByAnotherDefinitionSpec :: Spec
excludeByAnotherDefinitionSpec = describe "expr | exclude(anotherDefinition)" $
  it "excludes by another definition's own key set, and the removal survives a second for" $
    run (testStack $ do
      seedBranch "main"
        [ ("lore/notes.md", "a hand-authored note")
        , ("other.md", "not lore, should survive")
        , ("chapters/ch1.md", "chapter one prose")
        ]
      runDslOn (BranchName "main") go)
    `shouldBe` Right (Map.fromList
      [ ("other.md", "not lore, should survive")
      , ("chapters/ch1.md", "chapter one prose")
      ])
  where
    go = do
      v <- excludeByAnotherDefinitionDsl (bval loreDsl)
      entryTexts <- mapM (\(k, act) -> (,) k . messagesText <$> (valueDefault =<< act)) (valueEntries v)
      pure (Map.fromList entryTexts)

-- | @> expr@ (not just a literal) lets one entry build the exact
--   "User header, then Assistant content" pairing a hand-written agent's
--   own message assembly would otherwise have to construct in Haskell --
--   see 'Storyteller.Writer.Agent.Write.buildChapterMessages''s own
--   earlier-chapters framing, the case that motivated widening @>@.
assistantWrapsExprDsl :: Action Value
assistantWrapsExprDsl = [dsl|
as "chapter":
  "## Chapter: ch1.md"
  > read ch1.md
|]

assistantWrapsExprSpec :: Spec
assistantWrapsExprSpec = describe "> <expr> (Assistant-tag a general expression, not just a literal)" $
  it "builds a User header + Assistant content pair in one entry's own message list" $
    run (testStack $ do
      seedBranch "main" [("ch1.md", "chapter one prose")]
      runDslOn (BranchName "main") go)
    `shouldBe` Right [User "## Chapter: ch1.md", Assistant "chapter one prose"]
  where
    go = do
      v <- assistantWrapsExprDsl
      Just chapterAction <- pure (lookup "chapter" (valueEntries v))
      chapter <- chapterAction
      valueDefault chapter

-- | @name@\/@abstract@: the "acquaintance blurb" is convention over
--   already-stored Markdown (the required leading @# Title@, then its
--   opening paragraph -- see @WRITER.md@), not an LLM call, so both are
--   plain filters over 'read'\'s own output.
exampleSheet :: Text
exampleSheet = T.unlines
  [ "# Jenny"
  , ""
  , "Jenny is a girl who likes to read."
  , ""
  , "Tags: Student, Human"
  ]

nameFilterDsl :: Action Value
nameFilterDsl = [dsl|
read "sheet.md" | name
|]

abstractFilterDsl :: Action Value
abstractFilterDsl = [dsl|
read "sheet.md" | abstract
|]

nameAbstractFilterSpec :: Spec
nameAbstractFilterSpec = describe "expr | name / expr | abstract (Markdown convention, not an LLM call)" $ do
  it "name extracts the leading # heading" $
    run (testStack $ do
      seedBranch "main" [("sheet.md", exampleSheet)]
      runDslOn (BranchName "main") (messagesText <$> (valueDefault =<< nameFilterDsl)))
    `shouldBe` Right "Jenny"
  it "abstract extracts the paragraph immediately following the heading" $
    run (testStack $ do
      seedBranch "main" [("sheet.md", exampleSheet)]
      runDslOn (BranchName "main") (messagesText <$> (valueDefault =<< abstractFilterDsl)))
    `shouldBe` Right "Jenny is a girl who likes to read."

-- | Seeds a character branch with a sheet, an unrelated file (for
--   'contextCharacterSpec''s @"full"@ bucket), and a journal with one
--   entry that's a byte-identical copy of what it references (dropped by
--   'Storage.Tick.recentAtomsOf') and one that's genuinely diverged
--   (kept) -- the exact seeding shape "Storage.TickSpec" uses for
--   'Storage.Tick.recentAtomsOf' itself, replayed here through
--   'runBranchOpGit' since 'journalDelta' is exercised end-to-end via
--   the DSL, not called directly.
seedCharacterJournalBranch :: Text -> Sem (StoryStorage : TestEffects '[]) ()
seedCharacterJournalBranch name = do
  _ <- createBranch (BranchName name)
  runBranchOpGit @Main (BranchName name) $ do
    _  <- runStorage @Main (Ops.addAtom "sheet.md" exampleSheet)
    _  <- runStorage @Main (Ops.addAtom "extra.md" "some lore about Jenny")
    s1 <- runStorage @Main (Ops.addAtom "scene.md" "s1 content")
    _  <- runStorage @Main (Ops.addAtomWithRefs [s1] "journal.md" "s1 content")
    s2 <- runStorage @Main (Ops.addAtom "scene.md" "s2 content")
    _  <- runStorage @Main (Ops.addAtomWithRefs [s2] "journal.md" "s2 content, but Jenny remembers it differently")
    pure ()

journalDeltaDsl :: Binding -> Action Value
journalDeltaDsl = [dsl|
journal:
  journal "jenny"
|]

journalDeltaSpec :: Spec
journalDeltaSpec = describe "journalDelta (host-supplied Binding wrapping recentAtomsOf)" $
  it "reads the named character's own branch and drops entries unchanged from their own ref" $
    run (testStack $ do
      seedBranch "main" []
      seedCharacterJournalBranch "character/jenny"
      runDslOn (BranchName "main")
        (messagesText <$> (valueDefault =<< journalDeltaDsl (journalDelta 30 10 0))))
    `shouldBe` Right
      ( "### From this character's own journal (their private viewpoint -- may be biased, outdated, or contradict the wider record)\n\n"
        <> "s2 content, but Jenny remembers it differently"
      )

-- | End-to-end: 'CtxLibrary.contextCharacter', fully applied the same
--   way 'CtxLibrary.contextCharacterDefault' composes it in production,
--   exercised against a real character branch -- checks all five
--   buckets plus the "default is the blurb" behaviour the project chat
--   that designed this settled on. @"journalFull"@ vs. @"journal"@ is the
--   interesting pair: same underlying @journal.md@, uncurated
--   (everything, including the entry that's byte-identical to its own
--   ref) vs. curated (that duplicate dropped) -- proving the two really
--   are independent reads, not one derived from the other's already-
--   filtered result.
contextCharacterSpec :: Spec
contextCharacterSpec = describe "contextCharacter (sheet/blurb/full/journal/journalFull buckets)" $
  it "exposes each bucket independently, with blurb as the whole definition's own default" $
    run (testStack $ do
      seedBranch "main" []
      seedCharacterJournalBranch "character/jenny"
      runDslOn (BranchName "main") go)
    `shouldBe` Right
      ( "Jenny: Jenny is a girl who likes to read."
      , exampleSheet
      , "Jenny: Jenny is a girl who likes to read."
      , ["extra.md"]
      , "### From this character's own journal (their private viewpoint -- may be biased, outdated, or contradict the wider record)\n\n"
        <> "s2 content, but Jenny remembers it differently"
      , "s1 contents2 content, but Jenny remembers it differently"
      )
  where
    go = do
      v <- CtxLibrary.contextCharacter "jenny" (journalDelta 30 10 0)
      def <- messagesText <$> valueDefault v
      Just sheetAction <- pure (lookup "sheet" (valueEntries v))
      sheet <- messagesText <$> (valueDefault =<< sheetAction)
      Just blurbAction <- pure (lookup "blurb" (valueEntries v))
      blurb <- messagesText <$> (valueDefault =<< blurbAction)
      -- "full" also picks up 'scene.md' (a throwaway ref target for the
      -- journal fixture, not otherwise interesting here) -- only assert
      -- 'sheet.md'/'journal.md' stay excluded and 'extra.md' shows up.
      Just fullAction <- pure (lookup "full" (valueEntries v))
      full <- fullAction
      let fullKeys = List.sort (map fst (valueEntries full))
      Just journalAction <- pure (lookup "journal" (valueEntries v))
      journal <- messagesText <$> (valueDefault =<< journalAction)
      Just journalFullAction <- pure (lookup "journalFull" (valueEntries v))
      journalFull <- messagesText <$> (valueDefault =<< journalFullAction)
      pure (def, sheet, blurb, filter (== "extra.md") fullKeys, journal, journalFull)
