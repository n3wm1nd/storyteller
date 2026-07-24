{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Storyteller.Core.Git (runBranchAndFS, runStorage)
import Storyteller.Core.Storage (createBranch)
import Storyteller.Core.Types (BranchName(..))
import qualified Storage.Ops as Ops

import Server.Core.Branch (Main)
import Server.TestStack
import Storyteller.Writer.Agent.ContextCost

spec :: Spec
spec = describe "buildProgramCosts" $ do

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
