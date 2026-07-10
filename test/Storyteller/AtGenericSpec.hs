{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Tests for 'Storyteller.Core.Git.atGeneric' -- the generic, 'Sem'-level
--   rebase behind the rebase-marker feature, whose inner action runs
--   arbitrary effects (LLM calls, further dispatches) at an earlier tick's
--   position. 'atGeneric' descends one tick per 'runStorage' dispatch, runs
--   the inner action at the target, then replays the tail back on top in a
--   single batched 'StoreT' dispatch.
--
--   What that amounts to /externally/ is entirely a property of the
--   'withStorage' transaction 'atGeneric' always runs under in production
--   (see the command handlers): every intermediate 'setRef' is buffered in
--   'withStorageWithCallback''s overlay, and only the final replayed head
--   is published -- one ref write reaching git, regardless of how many
--   descent\/replay steps ran. A failure mid-rebase publishes nothing at
--   all (the replay never runs). Both are checked here, plus an eager-mode
--   contrast proving the single write is the transaction collapsing real
--   intermediate writes, not 'atGeneric' happening to make only one.
module Storyteller.AtGenericSpec (spec) where

import Prelude hiding (readFile, writeFile)

import qualified Data.ByteString as BS
import Control.Monad.State.Strict (lift)
import Data.Either (isLeft)
import Data.Text (Text)
import Test.Hspec

import Polysemy
import Polysemy.Fail (Fail(..), runFail)
import Polysemy.State (State, evalState, get, modify)

import Git.Mock (emptyGitState, runGitMock)
import Runix.Git

import Storyteller.Core.Types (BranchName(..), TickId(..), branchHead)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch, setRef)
import Storyteller.Core.Branch (BranchOp, runStorage)
import qualified Data.Map.Strict as Map

import Storyteller.Core.Git
  ( atGeneric, atGenericSeeded, runBranchOpGit, runStoryStorageGit, storyRefPrefix, withStorage )
import qualified Storage.Core as Core

-- | Local phantom branch tag. 'atGeneric'/'runBranchOpGit'/'runStorage' are
--   polymorphic over it, so the test needn't import
--   "Storyteller.Core.Runtime"'s 'Main' (and the deps it drags in).
data Main

mainBranch :: BranchName
mainBranch = BranchName "main"

mainRef :: RefName
mainRef = RefName (storyRefPrefix <> "main")

-- | Every real (mock-)git ref mutation that reached the 'Git' effect, in
--   order. A forwarding interpreter: reads and object I\/O pass through
--   untouched, only ref mutations ('CreateRef'\/'UpdateRef'\/'DeleteRef')
--   are recorded -- so this captures what actually landed in git, never
--   the in-memory overlay 'withStorage' buffers writes in before its
--   end-of-transaction replay. Sits between 'runStoryStorageGit' and
--   'runGitMock' so it sees the 'UpdateRef' 'applyToGit' issues on replay.
--   Also tallies 'IsAncestorOfAny' calls into a separate 'State Int' --
--   that effect is issued only by 'rewriteRef' inside 'cascadeReplace', so
--   the count is exactly the cross-branch cascade's per-branch sweeps.
type RefLog = [(RefName, Maybe ObjectHash)]

recordRefWrites :: Members '[Git, State RefLog, State Int] r => Sem (Git : r) a -> Sem r a
recordRefWrites = interpret $ \case
  CreateRef ref h      -> send (CreateRef ref h)  <* modify (++ [(ref, Just h)])
  UpdateRef ref h      -> send (UpdateRef ref h)  <* modify (++ [(ref, Just h)])
  DeleteRef ref        -> send (DeleteRef ref)    <* modify (++ [(ref, Nothing)])
  ResolveRef ref       -> send (ResolveRef ref)
  ListRefs p           -> send (ListRefs p)
  ReadCommit h         -> send (ReadCommit h)
  WriteCommit cd       -> send (WriteCommit cd)
  ReadObject h         -> send (ReadObject h)
  WriteObject obj      -> send (WriteObject obj)
  LookupPath t path    -> send (LookupPath t path)
  IsAncestorOfAny ts h -> modify @Int (+ 1) >> send (IsAncestorOfAny ts h)

writesOn :: RefName -> RefLog -> [Maybe ObjectHash]
writesOn ref = fmap snd . filter ((== ref) . fst)

-- | Build root <- a1 <- a2 <- a3 on 'mainBranch' (head a3), eagerly: each
--   store publishes immediately, /before/ the ref-write recording window
--   opens. Returns (a1 = rebase target, a3 = original head).
buildChain
  :: Members '[StoryStorage, Git, Fail] r
  => Sem r (Core.ObjectHash, Core.ObjectHash)
buildChain = do
  _ <- createBranch mainBranch
  (a1, _) <- runBranchOpGit @Main mainBranch (runStorage @Main (Core.store (Core.Atom [] "a.md" [] "A\n")))
  _ <- runBranchOpGit @Main mainBranch (runStorage @Main (Core.store (Core.Atom [] "a.md" [] "B\n")))
  (a3, _) <- runBranchOpGit @Main mainBranch (runStorage @Main (Core.store (Core.Atom [] "a.md" [] "C\n")))
  return (a1, a3)

-- | Descend from head a3 to a1, run @inner@ there, replay a2\/a3 back --
--   under one 'withStorage'. Returns the original head, the ref writes
--   that reached git /during the transaction/ (setup writes excluded), the
--   transaction's own result ('Left' iff @inner@ failed -- an inner
--   'runFail' so a mid-rebase failure is observed as a value, not a
--   short-circuit that hides the ref log), and the branch head afterwards.
runUnderWithStorage
  :: (forall r. Members '[BranchOp Main, Git, Fail] r => Sem r ())
  -> Either String (Core.ObjectHash, [Maybe ObjectHash], Either String (), Maybe TickId, Int)
runUnderWithStorage inner =
  run
  . runFail
  . evalState emptyGitState
  . evalState ([] :: RefLog)
  . evalState (0 :: Int)
  . runGitMock
  . recordRefWrites
  . runStoryStorageGit
  $ do
    (target, origHead) <- buildChain
    cascadesBefore <- get @Int
    logBefore <- get @RefLog
    txRes  <- runFail $ withStorage $ runBranchOpGit @Main mainBranch $
               atGeneric @Main (TickId (Core.unObjectHash target)) inner
    logAfter  <- get @RefLog
    cascadesAfter <- get @Int
    finalHead <- (branchHead <$>) <$> getBranch mainBranch
    return (origHead, writesOn mainRef (drop (length logBefore) logAfter), txRes, finalHead,
            cascadesAfter - cascadesBefore)

-- | Same rebase, but with NO 'withStorage' -- eager mode, where every
--   internal 'setRef' lands in git immediately. Returns the ref writes
--   made during the rebase itself (setup excluded).
runEager
  :: (forall r. Members '[BranchOp Main, Git, Fail] r => Sem r ())
  -> Either String [Maybe ObjectHash]
runEager inner =
  run
  . runFail
  . evalState emptyGitState
  . evalState ([] :: RefLog)
  . evalState (0 :: Int)
  . runGitMock
  . recordRefWrites
  . runStoryStorageGit
  $ do
    (target, _origHead) <- buildChain
    logBefore <- get @RefLog
    _ <- runBranchOpGit @Main mainBranch $
           atGeneric @Main (TickId (Core.unObjectHash target)) inner
    logAfter <- get @RefLog
    return (writesOn mainRef (drop (length logBefore) logAfter))

-- | 'Just' the hash text a single recorded write landed, for comparing a
--   ref value against the 'TickId' 'getBranch' reports.
singleWriteText :: [Maybe ObjectHash] -> Maybe Text
singleWriteText [Just h] = Just (unObjectHash h)
singleWriteText _        = Nothing

-- | Inner action that changes the working tree at the rebase target, so the
--   replayed ticks differ from the originals (newA2\/newA3 != a2\/a3).
--   Without this the replay reproduces identical content-addressed commits
--   and the remap is identity -- no real cross-branch work to verify.
markerInner :: forall r. Members '[BranchOp Main, Git, Fail] r => Sem r ()
markerInner = () <$ runStorage @Main (Core.store (Core.Atom [] "m.md" [] "M\n"))

journalBranch :: BranchName
journalBranch = BranchName "journal"

-- | Rebase 'mainBranch' from a3 to a1 with 'markerInner' (so a3 really does
--   get a new hash, not just an identity replay -- see its own Haddock),
--   producing main's own old->new mapping; then build a separate
--   'journalBranch' with a tick whose cross-branch ref points at the
--   *original* a3, and replay that branch's own tail (from its own first
--   tick) seeded with main's mapping -- the same two-branch shape
--   'Server.Writer.File.Dispatch's multi-branch 'At' case runs in
--   production, just with 'atGenericSeeded' called directly instead of
--   through 'runBranchAndFS' for a dynamically-named branch. Returns the
--   journal tick's ref after replay, and what a3 became, so the test can
--   check they now agree.
runCrossBranchSeededRemap :: Either String (Core.ObjectHash, Core.ObjectHash)
runCrossBranchSeededRemap =
  run
  . runFail
  . evalState emptyGitState
  . evalState ([] :: RefLog)
  . evalState (0 :: Int)
  . runGitMock
  . recordRefWrites
  . runStoryStorageGit
  $ do
    (target, origA3) <- buildChain
    _ <- createBranch journalBranch
    (j1, _) <- runBranchOpGit @Main journalBranch (runStorage @Main (Core.store (Core.NonAtom [] "type:note\nj1")))
    _ <- runBranchOpGit @Main journalBranch
           (runStorage @Main (Core.store (Core.NonAtom [origA3] "type:note\nabout scene's a3")))
    (_, mainMapping) <- runBranchOpGit @Main mainBranch $
      atGenericSeeded @Main mempty (TickId (Core.unObjectHash target)) markerInner
    _ <- runBranchOpGit @Main journalBranch $
      atGenericSeeded @Main mainMapping (TickId (Core.unObjectHash j1)) (pure ())
    journalHead <- (branchHead <$>) <$> getBranch journalBranch
    case journalHead of
      Nothing -> fail "journal branch has no head"
      Just h  -> do
        (tick, _) <- runBranchOpGit @Main journalBranch $ runStorage @Main $
          lift (Core.readTick (Core.ObjectHash (unTickId h)))
        newA3 <- case Map.lookup origA3 mainMapping of
          Nothing -> fail "main's own rebase produced no mapping for a3"
          Just n  -> return n
        case Core.tickRefs tick of
          [refId] -> return (refId, newA3)
          other   -> fail ("expected exactly one ref, got " <> show (length other))

forkBranch :: BranchName
forkBranch = BranchName "fork"

-- | Fork 'forkBranch' at main's head (a3), rebase main to a1 with 'inner',
--   return both branches' heads afterwards. The fork sits exactly at the
--   deepest replayed tick a3, so under the batched replay it is rewritten
--   directly via the a3->newA3 mapping entry and lands on newA3 = main's
--   replayed head. (The old per-tick cascade reparented a3 onto newA2 first,
--   leaving the fork on a reparented copy a3' != newA3.)
runForkAtHead
  :: (forall r. Members '[BranchOp Main, Git, Fail] r => Sem r ())
  -> Either String (Maybe TickId, Maybe TickId, Either String ())
runForkAtHead inner =
  run
  . runFail
  . evalState emptyGitState
  . evalState ([] :: RefLog)
  . evalState (0 :: Int)
  . runGitMock
  . recordRefWrites
  . runStoryStorageGit
  $ do
    (target, origHead) <- buildChain
    setRef forkBranch (Just (TickId (Core.unObjectHash origHead)))
    txRes <- runFail $ withStorage $ runBranchOpGit @Main mainBranch $
               atGeneric @Main (TickId (Core.unObjectHash target)) inner
    mainHead <- (branchHead <$>) <$> getBranch mainBranch
    forkHead <- (branchHead <$>) <$> getBranch forkBranch
    return (mainHead, forkHead, txRes)

-- | Like 'runForkAtHead' but adds a commit f1 on top of the fork point, so
--   the fork's head is its own commit (not a remapped one). After the rebase
--   the cascade must reparent f1 onto newA3 -- transitively: f1 isn't in the
--   mapping, but its ancestry contains a3 which is. Returns main's head and
--   fork's head's first parent, which should coincide (both newA3).
runForkWithOwnCommit
  :: (forall r. Members '[BranchOp Main, Git, Fail] r => Sem r ())
  -> Either String (Maybe TickId, Maybe ObjectHash, Either String ())
runForkWithOwnCommit inner =
  run
  . runFail
  . evalState emptyGitState
  . evalState ([] :: RefLog)
  . evalState (0 :: Int)
  . runGitMock
  . recordRefWrites
  . runStoryStorageGit
  $ do
    (target, origHead) <- buildChain
    setRef forkBranch (Just (TickId (Core.unObjectHash origHead)))
    _ <- runBranchOpGit @Main forkBranch (runStorage @Main (Core.store (Core.Atom [] "f.md" [] "F\n")))
    txRes <- runFail $ withStorage $ runBranchOpGit @Main mainBranch $
               atGeneric @Main (TickId (Core.unObjectHash target)) inner
    mainHead <- (branchHead <$>) <$> getBranch mainBranch
    forkHead <- (branchHead <$>) <$> getBranch forkBranch
    forkParent <- case forkHead of
      Just h  -> do cd <- readCommit (ObjectHash (unTickId h))
                    return (case commitParents cd of (p : _) -> Just p; [] -> Nothing)
      Nothing -> return Nothing
    return (mainHead, forkParent, txRes)

-- | What the inner action sees of "a.md" at the rebase target a1, read two
--   ways: through the plain ambient file access every Writer command and
--   agent actually uses, and through 'Core.inWorktree' (the explicit
--   committed-snapshot read, as a sanity check that the chain really is
--   wound back to the target underneath).
runInnerAmbientRead :: Either String (BS.ByteString, BS.ByteString)
runInnerAmbientRead =
  run
  . runFail
  . evalState emptyGitState
  . evalState ([] :: RefLog)
  . evalState (0 :: Int)
  . runGitMock
  . recordRefWrites
  . runStoryStorageGit
  $ do
    (target, _origHead) <- buildChain
    runBranchOpGit @Main mainBranch $
      atGeneric @Main (TickId (Core.unObjectHash target)) $ do
        (ambient, _)   <- runStorage @Main (Core.readFile "a.md")
        (committed, _) <- runStorage @Main (Core.inWorktree (Core.readFile "a.md"))
        return (ambient, committed)

spec :: Spec
spec = describe "atGeneric" $ do

  it "under withStorage, publishes exactly one ref write (the final replayed head) despite one setRef per internal step" $
    case runUnderWithStorage (pure ()) of
      Left err -> expectationFailure err
      Right (_origHead, mainWrites, txRes, finalHead, _cascades) -> do
        txRes `shouldBe` Right ()
        length mainWrites `shouldBe` 1
        singleWriteText mainWrites `shouldBe` fmap unTickId finalHead

  it "under withStorage, the cross-branch cascade fires exactly once for the whole replay tail (not once per replayed tick)" $
    case runUnderWithStorage (pure ()) of
      Left err -> expectationFailure err
      Right (_origHead, _mainWrites, txRes, _finalHead, cascades) -> do
        txRes `shouldBe` Right ()
        cascades `shouldBe` 1

  it "without withStorage (eager), the same rebase publishes a ref write at every internal step" $
    case runEager (pure ()) of
      Left err -> expectationFailure err
      Right mainWrites -> mainWrites `shouldSatisfy` ((> 1) . length)

  it "under withStorage, a failure mid-rebase publishes nothing and leaves the head untouched" $
    case runUnderWithStorage (send (Fail "boom")) of
      Left err -> expectationFailure err
      Right (origHead, mainWrites, txRes, finalHead, _cascades) -> do
        txRes `shouldSatisfy` isLeft
        mainWrites `shouldBe` []
        fmap unTickId finalHead `shouldBe` Just (Core.unObjectHash origHead)

  it "a branch forked at the deepest replayed tick tracks the replayed commit, not a reparented copy" $
    case runForkAtHead markerInner of
      Left err -> expectationFailure err
      Right (mainHead, forkHead, txRes) -> do
        txRes `shouldBe` Right ()
        forkHead `shouldBe` mainHead

  it "a forked branch's own commit is reparented onto the replayed chain (parent rewritten)" $
    case runForkWithOwnCommit markerInner of
      Left err -> expectationFailure err
      Right (mainHead, forkParent, txRes) -> do
        txRes `shouldBe` Right ()
        (unObjectHash <$> forkParent) `shouldBe` (unTickId <$> mainHead)

  -- The rebase-marker contract is "run this command as if @target@ were
  -- HEAD" -- and commands read files through the plain ambient file
  -- operations (via 'runStoryFSGit'), not through 'Core.inWorktree'. The
  -- descent winds the chain back correctly ('Core.drop' per tick), but
  -- nothing re-syncs the ambient tree to the target's snapshot, so a plain
  -- read inside the inner action still sees the file as the *original
  -- head* left it in the scope's ambient tree -- an agent generating at a
  -- past tick would assemble its context from future content.
  it "the inner action's plain file read sees the target tick's content, not the original head's" $
    case runInnerAmbientRead of
      Left err -> expectationFailure err
      Right (ambient, committed) -> do
        committed `shouldBe` "A\n"   -- sanity: the chain really is wound back to a1
        ambient   `shouldBe` "A\n"   -- the contract under test

  -- The gap this closes: a connected branch's cross-branch ref (e.g. a
  -- character journal atom's ref into the scene, see
  -- 'Storyteller.Writer.Agent.Tracker') used to go stale the moment the
  -- scene it points at got rebased, because that branch's own replay never
  -- ran with the scene's remap table in scope. 'atGenericSeeded' fixes it
  -- by threading one branch's own resulting mapping into another's replay.
  it "atGenericSeeded fixes a connected branch's stale cross-branch ref after a sibling branch's rebase" $
    case runCrossBranchSeededRemap of
      Left err -> expectationFailure err
      Right (journalRef, newA3) -> journalRef `shouldBe` newA3
