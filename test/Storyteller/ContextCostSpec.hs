{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | 'Storyteller.Writer.Agent.ContextCost.buildProgramCosts' -- ablation
--   ("blank this line out, re-run, measure the shrinkage") rather than
--   static per-statement summation, so a filter's real, possibly
--   non-additive effect on a line's contribution (an @exclude@ shrinking
--   a key another statement also touches, a @sortBy@ reordering, ...) is
--   always reflected correctly without this module needing to know
--   anything about what any given filter does.
module Storyteller.ContextCostSpec (spec) where

import Test.Hspec

import Polysemy (run)

import Control.Monad (void)

import Storyteller.Core.Git (runBranchAndFS, runBranchOpGit, runStorage)
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Presence (enters, leaves)
import Storyteller.Writer.Types (Character(..), PresenceEvent(..))
import qualified Storage.Ops as Ops

import Server.Core.Branch (Main)
import Server.TestStack
import Storyteller.Writer.Agent.ContextCost

spec :: Spec
spec = do
  buildProgramCostsSpec
  buildAdhocProgramCostsSpec

buildProgramCostsSpec :: Spec
buildProgramCostsSpec = describe "buildProgramCosts" $ do

  it "assigns each bare statement a cost including the separator its removal also collapses" $
    -- renderText joins rcContent elements with a two-character "\n\n" --
    -- ablation measures the real, honest effect of the line being gone
    -- (baseline "aaaa\n\nbb" = 8 chars; dropping "aaaa" leaves "bb" = 2
    -- chars; dropping "bb" leaves "aaaa" = 4 chars), separator included,
    -- not just each string literal's own length in isolation.
    (run . testStack $ do
      _ <- createBranch (BranchName "story")
      runBranchAndFS @Main (BranchName "story") $
        buildProgramCosts @Main "target.md" "path:\n  \"aaaa\"\n  \"bb\"\n")
    `shouldBe`
      Right
        [ LineCost { lcLine = 2, lcCol = 3, lcChars = 6 }
        , LineCost { lcLine = 3, lcCol = 3, lcChars = 4 }
        ]

  it "a for-loop is itself a candidate (ablating the whole loop), alongside its own body line" $
    (run . testStack $ do
      _ <- createBranch (BranchName "story")
      runBranchAndFS @Main (BranchName "story") $ do
        _ <- runStorage @Main (Ops.addAtom "lore/a.md" "aaaa")
        _ <- runStorage @Main (Ops.addAtom "lore/b.md" "bb")
        buildProgramCosts @Main "target.md" "path:\n  for f in lore/*: read f\n")
    `shouldBe`
      Right
        [ LineCost { lcLine = 2, lcCol = 3, lcChars = 8 }   -- the whole `for` loop -- both matches, gone entirely
        , LineCost { lcLine = 2, lcCol = 20, lcChars = 8 }  -- its own `read f` body -- ablating it drops both iterations' emissions the same way (the loop's whole body is one statement, run per match)
        ]

  it "ablating a whole as/for/in statement drops it outright, never leaving a padding artifact behind" $
    -- Regression case for a real bug found while building this: an early
    -- version substituted a neutral empty-string statement in place of
    -- the ablated one rather than removing it from the block outright.
    -- That's *not* neutral for a container statement (`as`/`for`/`in`,
    -- none of which contribute to the enclosing default at all) --
    -- `renderText`'s own "\n\n"-separator join still counted one more
    -- list element, so ablating a whole `as` block that contributes
    -- nothing to the rendered text at all came back with a *negative*
    -- cost (looking like removing it made the output bigger). Both
    -- candidates here must come back exactly zero.
    (run . testStack $ do
      _ <- createBranch (BranchName "story")
      runBranchAndFS @Main (BranchName "story") $
        buildProgramCosts @Main "target.md" "path:\n  x = \"kept\"\n  as \"label\": x\n  x\n")
    `shouldBe`
      Right
        [ LineCost { lcLine = 2, lcCol = 7, lcChars = 4 }  -- x = "kept" -- SLet's own position is never a candidate; this is its body
        , LineCost { lcLine = 4, lcCol = 3, lcChars = 4 }  -- the bare `x` that actually reaches the model
        , LineCost { lcLine = 3, lcCol = 3, lcChars = 0 }  -- the whole `as "label": x` -- contributes only a named entry, never the default
        , LineCost { lcLine = 3, lcCol = 15, lcChars = 0 } -- just the `x` inside it -- same reason
        ]

  it "the real context.writer active-character loop costs zero even with a genuinely present character -- as's own content never reaches renderText" $
    -- Direct answer to "how can this come back 0 if there's a character
    -- in the scene": `charactersin path` really does find Aria here
    -- (same fixture as Storyteller.Context.DSL.LibrarySpec's own
    -- "exposes each active character as a named entry" test, which
    -- asserts contextWriter's valueDefault is unchanged by her presence)
    -- -- the zero isn't "no character found," it's that `as c: ...`
    -- (and everything inside it, including the `context.character c`
    -- call on its own line) only ever populates a *named entry*
    -- (rcEntries), never the enclosing default (rcContent) --
    -- renderText/ablation-cost only ever read rcContent. Real content,
    -- parked somewhere this measurement structurally never looks.
    (run . testStack $ do
      _ <- createBranch (BranchName "main")
      _ <- createBranch (BranchName "character/aria")
      _ <- runBranchOpGit @Main (BranchName "character/aria")
        (runStorage @Main (Ops.addAtom "sheet.md" "# Aria\n\nA wandering rogue."))
      runBranchAndFS @Main (BranchName "main") $ do
        runBranchOpGit @Main (BranchName "main") $
          void (enters @Main "chapters/ch2.md" (Character (BranchName "character/aria")))
        buildProgramCosts @Main "chapters/ch2.md"
          "path:\n  for c in (charactersin path):\n    as c: context.character c\n")
    `shouldBe`
      Right
        [ LineCost { lcLine = 2, lcCol = 3, lcChars = 0 }  -- the whole `for` loop
        , LineCost { lcLine = 3, lcCol = 5, lcChars = 0 }  -- `as c: context.character c` -- the whole as-block
        , LineCost { lcLine = 3, lcCol = 11, lcChars = 0 } -- `context.character c` alone -- a real, output-producing call, but its output lands in the `as` block's own entry, not the default
        ]

-- | 'buildAdhocProgramCosts' -- what a per-call @pinnedPrograms@ entry
--   (Server.Writer.File.Protocol's own @ChatWriter@) is actually measured
--   through now that @context.writer@ no longer accepts a whole-program
--   override: no slot identity, just the submitted program's own rendered
--   size, broken down per statement, the same ablation logic
--   'buildProgramCosts' uses underneath.
--
--   The @Maybe FilePath@ is the submitting surface's own "which file would
--   this run against" (see 'Storyteller.Core.Context.adhocArgs'): a
--   program takes as many arguments as it declares, so a 0-arity one
--   ignores it and a @path:@-headed one -- every user-defined agent's, and
--   @context.other@'s -- finally costs correctly instead of being rejected
--   for having a parameter at all.
buildAdhocProgramCostsSpec :: Spec
buildAdhocProgramCostsSpec = describe "buildAdhocProgramCosts" $ do
  it "measures a bare 0-arity literal the same way buildProgramCosts measures a 1-arity one" $
    (run . testStack $ do
      _ <- createBranch (BranchName "story")
      runBranchAndFS @Main (BranchName "story") $
        buildAdhocProgramCosts @Main "\"aaaa\"\n\"bb\"\n" Nothing)
    `shouldBe`
      Right
        [ LineCost { lcLine = 1, lcCol = 1, lcChars = 6 }
        , LineCost { lcLine = 2, lcCol = 1, lcChars = 4 }
        ]

  it "ignores a supplied path when the program declares no parameter" $
    (run . testStack $ do
      _ <- createBranch (BranchName "story")
      runBranchAndFS @Main (BranchName "story") $
        buildAdhocProgramCosts @Main "\"aaaa\"\n" (Just "chapters/ch1.md"))
    `shouldBe`
      Right [ LineCost { lcLine = 1, lcCol = 1, lcChars = 4 } ]

  -- The regression this parameter exists for: costing a `path:`-headed
  -- program used to fail outright, which the UI showed as an empty
  -- estimate -- indistinguishable from "this program costs nothing".
  it "binds a supplied path to a 1-arity program's own parameter" $
    (run . testStack $ do
      _ <- createBranch (BranchName "story")
      runBranchAndFS @Main (BranchName "story") $
        buildAdhocProgramCosts @Main "name:\n  \"got %name%\"\n" (Just "ch1.md"))
    `shouldBe`
      Right [ LineCost { lcLine = 2, lcCol = 3, lcChars = 10 } ]  -- "got ch1.md"

  it "still fails on a 1-arity program when the caller has no argument to give" $
    (run . testStack $ do
      _ <- createBranch (BranchName "story")
      runBranchAndFS @Main (BranchName "story") $
        buildAdhocProgramCosts @Main "name:\n  \"got %name%\"\n" Nothing)
    `shouldSatisfy` \case
      Left _  -> True
      Right _ -> False
