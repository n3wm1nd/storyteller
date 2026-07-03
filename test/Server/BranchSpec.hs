{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.BranchSpec (spec) where

import Data.List (nub, sort)
import qualified Data.Text as T
import Test.Hspec
import Test.QuickCheck

import Polysemy
import Polysemy.Fail (failToError)
import Polysemy.Error (runError)
import Polysemy.State (State, evalState, runState, modify, put)
import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, listAllFiles, writeFile)
import Runix.Git (Git(..))
import Runix.Logging (loggingNull)

import Git.Mock (GitState, emptyGitState, runGitMock)
import Storyteller.Git (BranchTag, runBranchAndFS, withStorage, runStoryStorageGit)
import Storyteller.Storage (StoryBranch, StoryStorage, createBranch, storeAs, store)
import Storyteller.Types

import Server.TestStack

import Server.Core.Branch
import Server.Core.Protocol (Update(..), WireTick(..))

import Prelude hiding (writeFile)

-- ---------------------------------------------------------------------------
-- Runner
--
-- SessionEffects requires Random/Sleep/Time/LLM which aren't needed by
-- the pure branch operations. We run only what addNote / moveTickInBranch /
-- deleteTickFromBranch / branchState actually touch.
--
-- Branch functions assume their scope ('BranchOpen') is already open, same
-- as a real connection: it's entered once here, wrapping the whole action,
-- rather than per call.
--
-- 'runner' (a 'TestRunner', see 'Server.TestStack') is threaded through
-- most tests below and, per 'test/Main.hs', each is run once eagerly and
-- once through 'Storyteller.Git.withStorage'. The two 'branchStateSince'
-- tests are pinned to eager 'testStack' explicitly, since they're about
-- eager, cross-scope semantics specifically (see their own comment). The
-- "under withStorage specifically" section near the bottom is pinned to
-- 'withStorage' for the opposite reason: those aren't "does this also hold
-- under withStorage" checks but tests about withStorage's own behavior
-- (single-command transactions, nested 'At' calls, aborted transactions).
-- ---------------------------------------------------------------------------

withBranch_
  :: TestRunner
  -> BranchName
  -> Sem ( StoryBranch Main
         : FileSystemWrite (BranchTag Main)
         : FileSystemRead  (BranchTag Main)
         : FileSystem      (BranchTag Main)
         : StoryStorage
         : TestEffects '[] ) a
  -> Either String a
withBranch_ runner name action = run $ runner $ do
  _ <- createBranch name
  runBranchAndFS @Main name action

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Simulate a write made by a wholly separate connection: opens its own
--   fresh 'Main' scope (its own 'WorkingTree' load + commit + head), nested
--   inside an already-open outer scope. The outer scope's own state was
--   loaded before this runs and, by design, won't see it — see
--   'Storyteller.Git.runStoryBranchGit' and the tests below.
externalWrite :: BranchName -> FilePath -> Sem (StoryStorage : TestEffects '[]) TickId
externalWrite name path = runBranchAndFS @Main name $ do
  writeFile @(BranchTag Main) path "content"
  store @Main "external write"

tickIds :: Update -> [T.Text]
tickIds = map wtTickId . updateTicks

tickKinds :: Update -> [T.Text]
tickKinds = map wtKind . updateTicks

headIsIn :: Update -> Bool
headIsIn upd = updateHead upd `elem` tickIds upd

-- | Counts every real 'CreateRef'/'UpdateRef' that reaches git — the same
--   observation point 'Server.Writer.Run.gitNotify' uses to decide when to
--   broadcast a ref-move notification. Mirrors gitNotify's shape (see
--   Server.Writer.Run) so it sits in the stack the same way: above the real git
--   backend, below 'runStoryStorageGit'.
countRefWrites :: Members '[Git, State Int] r => Sem (Git : r) a -> Sem r a
countRefWrites = interpret $ \case
  ResolveRef  ref       -> send (ResolveRef ref)
  DeleteRef   ref       -> send (DeleteRef ref)
  ListRefs    prefix    -> send (ListRefs prefix)
  ReadCommit  h         -> send (ReadCommit h)
  ReadObject  h         -> send (ReadObject h)
  WriteObject obj       -> send (WriteObject obj)
  LookupPath  tree path -> send (LookupPath tree path)
  WriteCommit cd        -> send (WriteCommit cd)
  CreateRef   ref h     -> send (CreateRef ref h) <* modify (+ (1 :: Int))
  UpdateRef   ref h     -> send (UpdateRef ref h) <* modify (+ (1 :: Int))

-- | Fetch the live chain and move one tick to a position derived from
--   (fromRaw, afterRaw) via mod, so any Int pair is a valid move: position
--   0 means "move to front", 1..n means "move after the nth-remaining
--   tick". Re-fetches ids fresh every call — this is one command's worth
--   of work, meant to be called once per simulated client click, the same
--   way a real client always re-fetches before issuing its next command
--   rather than reusing ids from before the previous one landed.
applyOneMove :: BranchOpen r => (Int, Int) -> Sem r ()
applyOneMove (fromRaw, afterRaw) = do
  (_, upd) <- branchState
  let ids = [ TickId (wtTickId t) | t <- updateTicks upd, wtKind t /= "root" ]
      n   = length ids
  if n < 2 then return () else do
    let tid     = ids !! (fromRaw `mod` n)
        rest    = filter (/= tid) ids
        afterI  = afterRaw `mod` (length rest + 1)
        after   = if afterI == 0 then Nothing else Just (rest !! (afterI - 1))
    moveTickInBranch tid after

-- ---------------------------------------------------------------------------
-- Specs
-- ---------------------------------------------------------------------------

spec :: TestRunner -> Spec
spec runner = do

  describe "branchState" $ do

    it "returns state after branch creation" $
      withBranch_ runner (BranchName "test") branchState
        `shouldSatisfy` either (const False) (const True)

    it "head is always a member of the tick list" $
      withBranch_ runner (BranchName "test") branchState
        `shouldSatisfy` either (const False) (headIsIn . snd)

    it "fresh branch contains only the root tick" $ do
      let result = withBranch_ runner (BranchName "test") branchState
      case result of
        Left err       -> expectationFailure err
        Right (_, upd) -> tickKinds upd `shouldBe` ["root"]

  describe "branchStateSince" $ do

    -- These two are pinned to the eager 'testStack' rather than threaded
    -- through 'runner': they test that one already-open scope doesn't see
    -- a write made by a separately-opened one, using a raw
    -- 'externalWrite' punched through the ambient effect stack to simulate
    -- that separate connection. Under 'testStackTransactional' that write
    -- would land in the *same* buffered transaction as the scope under
    -- test instead of a separate one, changing what's being tested
    -- entirely — this pair is inherently about eager, cross-scope
    -- semantics, not something 'withStorage' buffering changes.
    --
    -- 'runStoryBranchGit' takes its head as a point-in-time snapshot from
    -- whenever the scope was opened and never reaches back out afterwards —
    -- deliberately: it's what makes the 'withStorage' transaction boundary
    -- and this scope's snapshot semantics agree, both syncing exactly once,
    -- at open (see the docs on 'Storyteller.Git.runStoryBranchGit'). So a
    -- still-open scope does not see a write made by a separately-opened
    -- one, for either the raw 'WorkingTree' (already true before) or
    -- 'branchStateSince' (StoryStorage-backed, previously always live).
    -- Freshness comes from reopening the scope, not from re-reading within
    -- an already-open one — see 'Server.Writer.Branch.Connection's notifier,
    -- which now reopens per notification for exactly this reason.
    it "a still-open scope does not see a write made by a separately-opened one" $ do
      let result = withBranch_ testStack (BranchName "test") $ do
            -- scope A "opens" here — its head and WorkingTree are snapshotted now
            (before, _) <- branchStateSince Nothing
            -- another connection writes a file via its own, separate scope
            _ <- raise . raise . raise . raise $ externalWrite (BranchName "test") "new.txt"
            stillTree <- listAllFiles @(BranchTag Main) "/"
            (stillSince, _) <- branchStateSince Nothing
            return (before, stillTree, stillSince)
      case result of
        Left err                              -> expectationFailure err
        Right (before, stillTree, stillSince) -> do
          before     `shouldNotContain` ["new.txt"]
          stillTree  `shouldNotContain` ["new.txt"]
          stillSince `shouldNotContain` ["new.txt"]

    it "reopening the scope sees a write made while it was closed" $ do
      let result = run $ testStack $ do
            _ <- createBranch (BranchName "test")
            _ <- externalWrite (BranchName "test") "new.txt"
            runBranchAndFS @Main (BranchName "test") (fst <$> branchStateSince Nothing)
      result `shouldBe` Right ["new.txt"]

  describe "addNote" $ do

    it "fails when the ref tick does not exist" $
      withBranch_ runner (BranchName "test") (addNote [TickId "nonexistent"] "hello")
        `shouldSatisfy` \case { Left _ -> True; Right _ -> False }

    it "produces a note tick visible in branchState" $ do
      let result = withBranch_ runner (BranchName "test") $ do
            -- root tick is the only tick; use its id as the ref
            (_, upd) <- branchState
            let refId = TickId (updateHead upd)
            addNote [refId] "a note"
            tickKinds . snd <$> branchState
      case result of
        Left err    -> expectationFailure err
        Right kinds -> kinds `shouldContain` ["note"]

    it "head still points to a valid tick after adding a note" $ do
      let result = withBranch_ runner (BranchName "test") $ do
            (_, upd) <- branchState
            addNote [TickId (updateHead upd)] "note"
            branchState
      case result of
        Left err       -> expectationFailure err
        Right (_, upd) -> headIsIn upd `shouldBe` True

  describe "deleteTickFromBranch" $ do

    it "deleted tick no longer appears in branchState" $ do
      let result = withBranch_ runner (BranchName "test") $ do
            (_, upd) <- branchState
            let refId = TickId (updateHead upd)
            noteId <- storeAs @Main (Note [refId] "to delete")
            deleteTickFromBranch noteId
            tickKinds . snd <$> branchState
      case result of
        Left err    -> expectationFailure err
        Right kinds -> kinds `shouldNotContain` ["note"]

  describe "moveTickInBranch" $ do

    it "chain length is unchanged after a move" $ do
      let result = withBranch_ runner (BranchName "test") $ do
            (_, upd) <- branchState
            let refId = TickId (updateHead upd)
            n1 <- storeAs @Main (Note [refId] "note1")
            _  <- storeAs @Main (Note [refId] "note2")
            before <- length . updateTicks . snd <$> branchState
            moveTickInBranch n1 Nothing
            after <- length . updateTicks . snd <$> branchState
            return (before, after)
      case result of
        Left err     -> expectationFailure err
        Right (b, a) -> b `shouldBe` a

    -- A sequence of moves, all within one test/transaction (see 'runner',
    -- 'Server.TestStack'). Checks the property a report of "moving
    -- globbers the data and history" calls for: not just "chain length
    -- unchanged" but "no lost or duplicated content after N moves in a
    -- row" — every message from the original set, exactly once, no
    -- duplicate ids.
    it "a fixed sequence of 6 moves over 6 ticks loses nothing" $ do
      let branch = BranchName "seqmove"
          result = withBranch_ runner branch $ do
            mapM_ (\i -> store @Main (T.pack ("t" <> show i))) [1 .. (6 :: Int)]
            mapM_ applyOneMove [(5,1), (0,3), (2,0), (4,2), (1,5), (3,3)]
            (_, upd) <- branchState
            let final = [ (TickId (wtTickId t), wtMessage t)
                        | t <- updateTicks upd, wtKind t /= "root" ]
            if length (nub (map fst final)) /= length final
              then fail $ "duplicate tick id in final chain: " <> show (map fst final)
              else return (map snd final)
      fmap sort result `shouldBe` Right (map (\i -> T.pack ("t" <> show i)) [1 .. 6])

    it "QuickCheck: any sequence of moves preserves the exact message set, with no duplicate ids" $
      property $ \moves ->
        length (moves :: [(Int, Int)]) <= 15 ==>
          let branch = BranchName "seqmove-qc"
              result = withBranch_ runner branch $ do
                mapM_ (\i -> store @Main (T.pack ("t" <> show i))) [1 .. (6 :: Int)]
                mapM_ applyOneMove moves
                (_, upd) <- branchState
                let final = [ (TickId (wtTickId t), wtMessage t)
                            | t <- updateTicks upd, wtKind t /= "root" ]
                if length (nub (map fst final)) /= length final
                  then fail $ "duplicate tick id in final chain: " <> show (map fst final)
                  else return (map snd final)
          in case result of
               Left err   -> counterexample err False
               Right msgs -> counterexample (show msgs) $
                 sort msgs === sort (map (\i -> T.pack ("t" <> show i)) [1 .. 6 :: Int])

  -- These are pinned to 'withStorage' explicitly rather than threaded
  -- through 'runner': they're not "does this behavior also hold under
  -- withStorage" checks (that's what 'runner' is for, above) but tests
  -- about 'withStorage' itself — the same distinction the parent
  -- conversation drew between "run the existing suite under withStorage
  -- too" and "also have dedicated tests for withStorage's own semantics
  -- (nesting, aborted transactions)".
  describe "moveTickInBranch under withStorage specifically" $ do

    -- Regression: a real client only ever sees a move's effect through a
    -- single command wrapped in exactly one 'withStorage' transaction (see
    -- 'Server.Writer.Branch.Connection.commandLoop'), with the branch scope
    -- reopened fresh for *that* command specifically — not reused from
    -- whatever scope did the seeding. Seeding and the move must be
    -- genuinely separate commands (separate 'runBranchAndFS' scopes, the
    -- seed committed for real before the move's own transaction opens),
    -- or this doesn't reproduce: bundling both into one shared scope/
    -- transaction is a coarser scenario that let a real corruption slip
    -- through here once already (see git history) — reported live as
    -- moving a tick in the Ticks view corrupting the chain: a real repo
    -- with atoms line1/line2/line3, after moving line2 to sit after line3,
    -- ended up with line3 committed *twice* under different ids while the
    -- file content itself still happened to read correctly. 'moveTick'
    -- nests two 'At' calls (see 'Storyteller.Edit.moveTick'); running that
    -- nesting through 'withStorage's buffering, in its own transaction,
    -- duplicates a tick in the resulting chain.
    it "a single move preserves content and produces no duplicate ids" $ do
      let branch = BranchName "single-move-withstorage"
          result = run $ testStack $ do
            _ <- createBranch branch
            runBranchAndFS @Main branch $
              mapM_ (\i -> store @Main (T.pack ("t" <> show i))) [1 .. (6 :: Int)]
            _ <- withStorage $ runBranchAndFS @Main branch $ applyOneMove (0, 1)
            runBranchAndFS @Main branch $ do
              (_, upd) <- branchState
              let final = [ (TickId (wtTickId t), wtMessage t)
                          | t <- updateTicks upd, wtKind t /= "root" ]
              if length (nub (map fst final)) /= length final
                then fail $ "duplicate tick id in final chain: " <> show (map fst final)
                else return (map snd final)
      result `shouldBe` Right (["t2","t1","t3","t4","t5","t6"] :: [T.Text])

    -- Regression for a duplication bug in the frontend Ticks view: runs
    -- moveTickInBranch the way the real server actually does — nested
    -- inside 'withStorage', the way 'Server.Writer.Branch.Connection.commandLoop'
    -- wraps every command — and counts how many real git ref writes that
    -- one logical move produces. Before the fix in
    -- 'Storyteller.Git.withStorage', a move nesting two 'At' calls plus a
    -- multi-entry 'updateReferences' cascade buffered one ref write per
    -- step and replayed every one of them individually — each a real,
    -- eagerly-visible git ref update and a real 'RefMoved' notification.
    -- Any branch connection's notifier thread (including the one that
    -- issued the move) reopens and re-pushes state on every such
    -- notification, so a client could observe and render the transient,
    -- half-rewritten intermediate chain before the final one landed.
    it "single moveTick produces exactly one real git ref write" $ do
      let result =
            run
            . runError @String
            . failToError id
            . loggingNull
            . evalState emptyGitState
            . runState (0 :: Int)
            . runGitMock
            . countRefWrites
            . runStoryStorageGit
            $ do
                _ <- createBranch (BranchName "test")
                n1 <- runBranchAndFS @Main (BranchName "test") $ do
                  (_, upd) <- branchState
                  let refId = TickId (updateHead upd)
                  n1 <- storeAs @Main (Note [refId] "note1")
                  _  <- storeAs @Main (Note [refId] "note2")
                  return n1
                put (0 :: Int)
                -- 'withStorage' outermost, the branch scope opened inside
                -- it — matching how 'Server.Writer.Branch.Connection.commandLoop'
                -- actually wraps every command. A scope opened *before*
                -- 'withStorage' resolves its internal ref writes against
                -- the outer, unbuffered StoryStorage, bypassing the
                -- buffering entirely and defeating the test.
                _ <- withStorage $ runBranchAndFS @Main (BranchName "test") $
                  moveTickInBranch n1 Nothing
                return ()
      case result of
        Left err        -> expectationFailure err
        Right (n, ())    -> n `shouldBe` (1 :: Int)

    -- A command that fails partway through a move should leave the branch
    -- exactly as it was — 'withStorage' only replays its buffered writes
    -- once the wrapped action succeeds (see 'Storyteller.Git.withStorage'),
    -- so nothing should reach real git at all. Threads 'GitState' by hand
    -- across separate runs (setup, the aborted attempt, two after-checks)
    -- rather than one shared 'Sem' computation, so the aborted attempt's
    -- failure can be observed as a plain 'Left' without also unwinding the
    -- state 'setup' built. The re-check after the aborted attempt reuses
    -- 'gs1' (the branch's state from just before that attempt) rather than
    -- whatever 'GitState' the failed run itself ended at: 'withStorage'
    -- guarantees the *ref* — the only thing 'branchState' ever reads — is
    -- untouched on failure (any orphaned commit objects the attempt wrote
    -- before failing are inert), so reading from either is equally valid;
    -- 'gs1' avoids extracting a result out of a 'Left'.
    it "an aborted transaction leaves no trace" $ do
      let branch = BranchName "aborted"

          runGitState :: GitState -> Sem (TestEffects '[]) a -> Either String (GitState, a)
          runGitState gs0 action =
            run
            . runError @String
            . failToError id
            . loggingNull
            . runState gs0
            . runGitMock
            . runStoryStorageGit
            $ action

          Right (gs1, ()) = runGitState emptyGitState $ do
            _ <- createBranch branch
            runBranchAndFS @Main branch $
              mapM_ (\i -> store @Main (T.pack ("t" <> show i))) [1 .. (3 :: Int)]

          Right (_, before) = runGitState gs1 $ runBranchAndFS @Main branch branchState

          aborted = runGitState gs1 $ withStorage $
            runBranchAndFS @Main branch $ do
              _ <- applyOneMove (0, 1)
              fail "simulated failure after the move's writes were buffered"

          Right (_, after) = runGitState gs1 $ runBranchAndFS @Main branch branchState

      aborted `shouldSatisfy` \case { Left _ -> True; Right (_ :: (GitState, ())) -> False }
      after `shouldBe` before
