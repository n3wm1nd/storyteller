{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | 'Server.Writer.File.Connection.openTarget' is the one place a
-- @name\@kind@ connection target is told apart from a plain branch name --
-- everything downstream ('Server.Core.File.fileStateSince'\/'editFileAtom',
-- 'Server.Writer.File.Dispatch.runCommand') is the exact same code either
-- way. What this pins:
--
--   * a plain name still opens the real branch, unaffected;
--   * @name\@kind@ opens whatever @kind@'s *current* 'Summary' tick names
--     -- resolved fresh on every call, never cached, so a later
--     re-summarize pass is visible on the very next 'openTarget' call with
--     no special notification path needed (the live-connection guarantee
--     every other target already has);
--   * writing through @name\@kind@ lands in the alternate chain, mints a
--     fresh 'Summary' tick recording it, and leaves the real file's own
--     content completely untouched.
--
-- Each 'openTarget' call below opens and closes its own scope, deliberately
-- never one long-lived scope shared across the test -- exactly the
-- "reopen fresh per command" discipline the real connection uses (see
-- 'Server.Writer.File.Connection.commandLoop'), which is also what makes
-- the "resolved fresh, never cached" claim above actually meaningful to
-- test rather than assumed.
module Server.Writer.File.ConnectionSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec

import Polysemy (Sem, run)

import Storyteller.Core.Git (atGeneric, runStorage, withStorage)
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types (BranchName(..), TickId(..))
import Storyteller.Common.Summary (Summary(..), lastSummaryOf)
import Storyteller.Writer.Agent.Summarizer (runSummarizer)
import Storyteller.Writer.Agent.JournalSummarizer (journalSummarize, journalKind, defaultJournalGroupSize)
import Storyteller.Writer.Library (journalPath)
import Storyteller.Core.Runtime (Main)
import Storage.Core (unObjectHash)
import qualified Storage.Ops as Ops

import Server.Core.File (fileStateSince, editFileAtom)
import Server.Core.Protocol (Update(..), WireTick(..))
import Server.Writer.File (fileStateWithSummaries, summarizePathManual)
import Server.Writer.File.Connection (openTarget)
import Storyteller.Writer.Agent.SummaryAccess (densest)
import Server.TestStack (TestRunner)

spec :: TestRunner -> Spec
spec runner = describe "openTarget" $ do
  let run_ action = run (runner action)
      -- Every real client command dispatches through exactly one
      -- 'withStorage' boundary per call (see
      -- 'Server.Writer.File.Connection.commandLoop's own 'handle') --
      -- 'openTarget'\/'atGeneric's own remap-table propagation is
      -- documented as "entirely the transaction boundary's business", so
      -- this alias is what every call below actually uses, matching that
      -- granularity exactly instead of the coarser "several calls sharing
      -- no boundary at all" shape a raw 'openTarget' would give a test.
      openCmd target = withStorage . openTarget target

  it "a plain name (no @kind) still opens the real branch's own content" $ do
    let result = run_ $ do
          _   <- createBranch (BranchName "b")
          _   <- openCmd "b" (runStorage @Main (Ops.addAtom "chapters/ch1.md" "raw v1."))
          upd <- openCmd "b" (fileStateSince "chapters/ch1.md" Nothing)
          return (map wtContent (updateTicks upd))
    result `shouldBe` Right [Just "raw v1."]

  it "name@kind for a never-summarized kind opens absent, then a write mints the first Summary tick -- manual summary creation" $ do
    let result = run_ $ do
          _          <- createBranch (BranchName "b")
          beforeTick <- openCmd "b" (runStorage @Main (lastSummaryOf "custom/notes"))
          upd0       <- openCmd "b@custom/notes" (fileStateSince "notes.md" Nothing)
          _          <- openCmd "b@custom/notes" (runStorage @Main (Ops.append "notes.md" "hand-written note"))
          upd1       <- openCmd "b@custom/notes" (fileStateSince "notes.md" Nothing)
          afterTick  <- openCmd "b" (runStorage @Main (lastSummaryOf "custom/notes"))
          return (beforeTick, updateHead upd0, map wtContent (updateTicks upd1), fmap (summaryKind . snd) afterTick)
    result `shouldBe` Right (Nothing, "", [Just "hand-written note\n"], Just "custom/notes")

  it "name@kind opens the alternate chain's own content, not the real file's" $ do
    let result = run_ $ do
          _   <- createBranch (BranchName "b")
          _   <- openCmd "b" (runStorage @Main (Ops.addAtom "chapters/ch1.md" "raw v1."))
          _   <- openCmd "b" (runSummarizer @Main "prose/chapter" (\_ -> pure (Map.singleton "chapters/ch1.md" "condensed v1")))
          upd <- openCmd "b@prose/chapter" (fileStateSince "chapters/ch1.md" Nothing)
          return (map wtContent (updateTicks upd))
    result `shouldBe` Right [Just "condensed v1"]

  it "resolves the current Summary tick fresh every call -- a later pass is visible with no caching" $ do
    let result = run_ $ do
          _    <- createBranch (BranchName "b")
          _    <- openCmd "b" (runStorage @Main (Ops.addAtom "chapters/ch1.md" "raw v1."))
          _    <- openCmd "b" (runSummarizer @Main "prose/chapter" (\_ -> pure (Map.singleton "chapters/ch1.md" "condensed v1")))
          upd1 <- openCmd "b@prose/chapter" (fileStateSince "chapters/ch1.md" Nothing)
          _    <- openCmd "b" (runStorage @Main (Ops.addAtom "chapters/ch1.md" "\n\nraw v2."))
          _    <- openCmd "b" (runSummarizer @Main "prose/chapter" (\_ -> pure (Map.singleton "chapters/ch1.md" "condensed v2")))
          upd2 <- openCmd "b@prose/chapter" (fileStateSince "chapters/ch1.md" Nothing)
          return (map wtContent (updateTicks upd1), map wtContent (updateTicks upd2))
    result `shouldBe` Right ([Just "condensed v1"], [Just "condensed v2"])

  it "a hand-edit must not silently claim coverage of raw content added after the tick it navigated from" $ do
    let result = run_ $ do
          _   <- createBranch (BranchName "b")
          _   <- openCmd "b" (runStorage @Main (Ops.addAtom "chapters/ch1.md" "para one."))
          _   <- openCmd "b" (runSummarizer @Main "prose/chapter" (\_ -> pure (Map.singleton "chapters/ch1.md" "condensed v1")))
          -- New raw content lands *after* the summary pass -- nothing has
          -- reprocessed it yet, so it must stay visible as an
          -- unsummarized tail no matter what happens to the summary next.
          _    <- openCmd "b" (runStorage @Main (Ops.addAtom "chapters/ch1.md" "\n\npara two, never seen by any pass."))
          -- A hand-edit through the summary tier that has no idea "para
          -- two" exists -- it only ever saw the alt chain's own content.
          -- Before 'mintSummaryTick' used 'atGeneric' to insert the new
          -- Summary tick at the *old* tick's own position (replaying
          -- "para two" back on top of it) rather than appending at
          -- whatever the branch's real head had since moved to, this
          -- hand note would have silently made "para two" permanently
          -- invisible to 'densest' -- not a display quirk, a real loss
          -- from every reader's own context assembly.
          _    <- openCmd "b@prose/chapter" (runStorage @Main (Ops.append "chapters/ch1.md" " (hand note)"))
          openCmd "b" (densest @Main ["prose/chapter"] "chapters/ch1.md")
    result `shouldBe` Right "condensed v1 (hand note)\n\n\npara two, never seen by any pass."

  it "editing through name@kind lands in the alternate chain, mints a fresh Summary tick, and never touches the real file" $ do
    let result = run_ $ do
          _         <- createBranch (BranchName "b")
          _         <- openCmd "b" (runStorage @Main (Ops.addAtom "chapters/ch1.md" "raw v1."))
          _         <- openCmd "b" (runSummarizer @Main "prose/chapter" (\_ -> pure (Map.singleton "chapters/ch1.md" "condensed v1")))
          Just (oldTickId, _) <- openCmd "b" (runStorage @Main (lastSummaryOf "prose/chapter"))
          upd0      <- openCmd "b@prose/chapter" (fileStateSince "chapters/ch1.md" Nothing)
          let [editTid] = map (TickId . wtTickId) (updateTicks upd0)
          _         <- openCmd "b@prose/chapter" (editFileAtom "chapters/ch1.md" editTid "hand-edited condensation")
          upd1      <- openCmd "b@prose/chapter" (fileStateSince "chapters/ch1.md" Nothing)
          realUpd   <- openCmd "b" (fileStateSince "chapters/ch1.md" Nothing)
          Just (newTickId, _) <- openCmd "b" (runStorage @Main (lastSummaryOf "prose/chapter"))
          return
            ( map wtContent (updateTicks upd1)
            , map wtContent (updateTicks realUpd)
            , oldTickId == newTickId
            )
    result `shouldBe` Right ([Just "hand-edited condensation"], [Just "raw v1."], False)

  -- The exact client flow the split view produces: click an occurrence's
  -- annotation (hops = its own real tick id), then append through the
  -- bottom pane. The write must supersede that occurrence *on the real
  -- branch* (the same insert-directly-after shape 'mintSummaryTick' uses,
  -- and exactly the superseding-occurrence pattern
  -- 'Storyteller.Common.Summary.summariesTouching' models first-class) --
  -- and the *same* hop target must then read the edit back: a hop names
  -- "this occurrence, in its current state" (resolution follows the
  -- superseding run's tip), not a frozen snapshot that would silently
  -- swallow the caller's own edit on the very next push.
  it "appending through a specific occurrence's own hop supersedes it on the real branch, and the same hop reads the edit back" $ do
    let result = run_ $ do
          _ <- createBranch (BranchName "b")
          _ <- openCmd "b" (runStorage @Main (Ops.addAtom "chapters/ch1.md" "raw v1."))
          _ <- openCmd "b" (runSummarizer @Main "prose/chapter" (\_ -> pure (Map.singleton "chapters/ch1.md" "condensed v1")))
          Just (tid, _) <- openCmd "b" (runStorage @Main (lastSummaryOf "prose/chapter"))
          let target = "b@prose/chapter#" <> unTickId tid
          _       <- openCmd target (runStorage @Main (Ops.append "chapters/ch1.md" "hand note"))
          upd     <- openCmd target (fileStateSince "chapters/ch1.md" Nothing)
          realUpd <- openCmd "b" (fileStateSince "chapters/ch1.md" Nothing)
          Just (newTid, _) <- openCmd "b" (runStorage @Main (lastSummaryOf "prose/chapter"))
          return (map wtContent (updateTicks upd), map wtContent (updateTicks realUpd), newTid /= tid)
    result `shouldBe` Right ([Just "condensed v1", Just "hand note\n"], [Just "raw v1."], True)

  -- Same flow one tier deeper: append through a *nested* hop
  -- ("b@journal#top#nested") and confirm the re-mint cascades all the way
  -- back out -- the nested tick superseded within its parent's own
  -- alt-chain, and a fresh top-level Summary tick minted on the real
  -- branch -- with the same nested hop target then reading the edit back.
  it "appending through a nested hop cascades the re-mint out to a fresh top-level Summary tick" $ do
    let n = defaultJournalGroupSize
        stubCompress :: [T.Text] -> Sem r T.Text
        stubCompress items = pure ("C[" <> T.intercalate "," items <> "]")
        result = run_ $ do
          _ <- createBranch (BranchName "b")
          mapM_ (\i -> openCmd "b" (runStorage @Main (Ops.addAtom journalPath (T.pack (show i))))) [1 .. n * n :: Int]
          _ <- openCmd "b" (journalSummarize @Main stubCompress)
          Just (topTid, _) <- openCmd "b" (runStorage @Main (lastSummaryOf journalKind))
          let target1 = "b@" <> journalKind <> "#" <> unTickId topTid
          (upd1, _) <- openCmd target1 (fileStateWithSummaries journalPath Nothing)
          case [wtTickId wt | wt <- updateTicks upd1, wtKind wt == "summary"] of
            [] -> fail "no nested occurrence surfaced on the tier-0 connection"
            (nestedTid : _) -> do
              let target2 = target1 <> "#" <> nestedTid
              _ <- openCmd target2 (runStorage @Main (Ops.append journalPath "tier-1 hand note"))
              (upd2, _) <- openCmd target2 (fileStateWithSummaries journalPath Nothing)
              Just (newTop, _) <- openCmd "b" (runStorage @Main (lastSummaryOf journalKind))
              return
                ( Just "tier-1 hand note\n" `elem` map wtContent (updateTicks upd2)
                , newTop /= topTid
                )
    result `shouldBe` Right (True, True)

  -- Pins the exact thing the client's own split-view connection does: open
  -- one specific top-level occurrence's own hop ("b@journal#<tid>", not the
  -- bare "b@journal" a fresh/live view would use) and confirm
  -- 'fileStateWithSummaries' -- the same call 'pushInitial'/'pushIncremental'
  -- make for *any* connection, at any depth -- still surfaces a nested
  -- tier's own occurrence riding along, exactly as it would from the bare
  -- top-level connection (Storyteller.JournalSummarizerSpec already pins
  -- this at the 'summariesTouching' level directly; this pins it at the
  -- full connection-opening layer the client actually goes through).
  it "opening a specific top-level occurrence's own hop still surfaces a nested tier's own occurrence" $ do
    let n = defaultJournalGroupSize
        stubCompress :: [T.Text] -> Sem r T.Text
        stubCompress items = pure ("C[" <> T.intercalate "," items <> "]")
        result = run_ $ do
          _ <- createBranch (BranchName "b")
          mapM_ (\i -> openCmd "b" (runStorage @Main (Ops.addAtom journalPath (T.pack (show i))))) [1 .. n * n :: Int]
          _ <- openCmd "b" (journalSummarize @Main stubCompress)
          Just (latestTid, _) <- openCmd "b" (runStorage @Main (lastSummaryOf journalKind))
          let target = "b@" <> journalKind <> "#" <> unTickId latestTid
          (upd, _sig) <- openCmd target (fileStateWithSummaries journalPath Nothing)
          return (map wtKind (updateTicks upd))
    case result of
      Left err    -> expectationFailure err
      Right kinds -> filter (== "summary") kinds `shouldSatisfy` (not . null)

  -- 'Server.Writer.File.summarizePathManual' -- create an empty occurrence
  -- for the user to write into directly, reusing the exact same
  -- coverage-finding machinery an automatic pass uses (no separate
  -- positioning logic of its own): 'runSummarizerForPath' for a whole-file
  -- kind, 'journalCreateManual' for the incremental one. "At the current
  -- cursor" is free -- the client wraps this command in 'At' like any
  -- other mutating command (see WS-PROTOCOL.md), so 'atGeneric' is what a
  -- rebase marker actually becomes server-side; exercised directly here,
  -- the same way 'Server.Writer.File.Dispatch.runCommand's own 'At' case
  -- wraps any inner command.
  describe "summarizePathManual" $ do
    it "creates one empty occurrence covering the file's current content, at the live end when no cursor is given" $ do
      let result = run_ $ do
            _   <- createBranch (BranchName "b")
            _   <- openCmd "b" (runStorage @Main (Ops.addAtom "chapters/ch1.md" "raw v1."))
            tid <- openCmd "b" (summarizePathManual "chapters/ch1.md")
            upd <- openCmd "b@prose/chapter" (fileStateSince "chapters/ch1.md" Nothing)
            return (tid /= Nothing, map wtContent (updateTicks upd))
      result `shouldBe` Right (True, [Just ""])

    it "wrapped in At (what a client's rebase marker becomes server-side) -- later real content stays unsummarized" $ do
      let result = run_ $ do
            _        <- createBranch (BranchName "b")
            cursor   <- openCmd "b" (runStorage @Main (Ops.addAtom "chapters/ch1.md" "para one."))
            -- Content written *after* the cursor -- the manual creation
            -- below, rebased to that earlier point exactly the way
            -- 'Server.Writer.File.Dispatch.runCommand's own 'At' case
            -- rebases any inner command, must never see this.
            _        <- openCmd "b" (runStorage @Main (Ops.addAtom "chapters/ch1.md" "\n\npara two, after the cursor."))
            _        <- openCmd "b" (atGeneric @Main (TickId (unObjectHash cursor)) (summarizePathManual "chapters/ch1.md"))
            openCmd "b" (densest @Main ["prose/chapter"] "chapters/ch1.md")
      -- The empty occurrence covers only "para one." (everything up to the
      -- cursor); "para two" -- written after it -- surfaces as densest's
      -- own unsummarized tail, exactly like the hand-edit test above pins
      -- for 'openTarget' generally, now confirmed for manual creation too.
      result `shouldBe` Right "\n\npara two, after the cursor."

    it "a second manual call with nothing new since is a genuine no-op, same contract as an automatic pass" $ do
      let result = run_ $ do
            _    <- createBranch (BranchName "b")
            _    <- openCmd "b" (runStorage @Main (Ops.addAtom "chapters/ch1.md" "raw v1."))
            tid1 <- openCmd "b" (summarizePathManual "chapters/ch1.md")
            tid2 <- openCmd "b" (summarizePathManual "chapters/ch1.md")
            return (tid1 /= Nothing, tid2)
      result `shouldBe` Right (True, Nothing)

    it "on journal.md, flushes whatever's pending right now even under a full group, and drops the user into a readable empty occurrence" $ do
      let result = run_ $ do
            _   <- createBranch (BranchName "b")
            mapM_ (\i -> openCmd "b" (runStorage @Main (Ops.addAtom journalPath (T.pack (show i))))) [1 .. 3 :: Int]
            tid <- openCmd "b" (summarizePathManual journalPath)
            upd <- openCmd ("b@" <> journalKind) (fileStateSince journalPath Nothing)
            return (tid, map wtContent (updateTicks upd))
      result `shouldBe` Right (Nothing, [Just ""])
