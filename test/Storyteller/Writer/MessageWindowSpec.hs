{-# LANGUAGE OverloadedStrings #-}

module Storyteller.Writer.MessageWindowSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import UniversalLLM (Message(..))

import Storyteller.Core.LLM.Role (ProseModel)
import Storyteller.Writer.Agent.MessageWindow (injectAtWindow, windowBoundary)

isUserTurn :: Message m -> Bool
isUserTurn (UserText _) = True
isUserTurn _            = False

-- | @n@ alternating turns, oldest first: @[UserText "p1", AssistantText
--   "r1", UserText "p2", ...]@.
turns :: Int -> [Message ProseModel]
turns n = concat
  [ [UserText ("p" <> T.pack (show i)), AssistantText ("r" <> T.pack (show i))]
  | i <- [1 .. n]
  ]

inject :: [Message ProseModel] -> [Message ProseModel] -> [Message ProseModel]
inject = injectAtWindow isUserTurn 2 4

spec :: Spec
spec = do
  describe "windowBoundary" $ do
    it "is 0 while total is at or below lo" $
      map (windowBoundary 2 4) [0, 1, 2] `shouldBe` [0, 0, 0]

    it "holds a single value across a whole (hi - lo + 1)-turn stretch, then jumps by that same amount" $
      map (windowBoundary 2 4) [2 .. 10] `shouldBe` [0, 0, 0, 3, 3, 3, 6, 6, 6]

    it "moves every turn when lo == hi -- the degenerate 'always exactly N deep' case" $
      map (windowBoundary 2 2) [2 .. 6] `shouldBe` [0, 1, 2, 3, 4]

  describe "injectAtWindow" $ do
    it "returns history unchanged when there's nothing to inject" $
      inject [] (turns 3) `shouldBe` turns 3

    it "with an empty history, the injected block is all there is" $
      inject [UserText "splice"] [] `shouldBe` [UserText "splice"]

    it "with fewer turns than lo, injects at the very front" $
      inject [UserText "splice"] (turns 1) `shouldBe` UserText "splice" : turns 1

    it "never splits a turn -- the injected block always lands between a reply and the next prompt" $ do
      let result = inject [UserText "splice"] (turns 5)
      -- boundary(2,4,5) = 3 turns before the split (see windowBoundary spec)
      result `shouldBe` take 6 (turns 5) ++ [UserText "splice"] ++ drop 6 (turns 5)

    it "gives a byte-identical prefix across turn counts that share the same boundary -- the actual cache-hit guarantee" $ do
      -- total=3 and total=4 both fall in the same window stretch (boundary
      -- 0 for both, see windowBoundary spec above), so injecting at 3 turns
      -- must be a strict prefix of injecting at 4 -- purely appending the
      -- new turn, never reshuffling what came before.
      let splice = [UserText "splice"]
          atThree = inject splice (turns 3)
          atFour  = inject splice (turns 4)
      atThree `shouldBe` take (length atThree) atFour
