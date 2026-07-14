{-# LANGUAGE OverloadedStrings #-}

module Storyteller.WorldContextSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Storage.MockStore (runChain)
import Storage.Ops (addAtom)

import Storyteller.Writer.Agent (ContextBlock(..))
import Storyteller.Writer.Agent.WorldContext (WorldLore(..), SystemContext(..), isSystemContextPath, worldContextOf)

fenced :: T.Text -> T.Text -> T.Text
fenced path content = "<context-file path=\"" <> path <> "\">\n" <> content <> "\n</context-file>"

spec :: Spec
spec = describe "worldContextOf" $ do

  it "puts root-level style.md in SystemContext, not WorldLore" $ do
    let result = fst <$> runChain (do
          _ <- addAtom "style.md" "Write in close third person.\n"
          worldContextOf)
    case result of
      Left err -> expectationFailure err
      Right (WorldLore lore, SystemContext sys) -> do
        lore `shouldBe` []
        sys `shouldBe` [ContextBlock (fenced "style.md" "Write in close third person.\n")]

  it "puts a hand-authored note in WorldLore, not SystemContext" $ do
    let result = fst <$> runChain (do
          _ <- addAtom "notes/the-guttering-candle.md" "A tavern in the old quarter.\n"
          worldContextOf)
    case result of
      Left err -> expectationFailure err
      Right (WorldLore lore, SystemContext sys) -> do
        sys `shouldBe` []
        lore `shouldBe` [ContextBlock (fenced "notes/the-guttering-candle.md" "A tavern in the old quarter.\n")]

  it "puts the whole-story outline and a chapter's own beat sheet in WorldLore -- neither is special, just an unclassified file that isn't a chapter or the style guide" $ do
    let result = fst <$> runChain (do
          _ <- addAtom "outline.md" "Act one: the ship departs.\n"
          _ <- addAtom "chapters/ch1.outline.md" "Beat one: arrival.\n"
          worldContextOf)
    case result of
      Left err -> expectationFailure err
      Right (WorldLore lore, SystemContext sys) -> do
        sys `shouldBe` []
        lore `shouldBe`
          [ ContextBlock (fenced "chapters/ch1.outline.md" "Beat one: arrival.\n")
          , ContextBlock (fenced "outline.md" "Act one: the ship departs.\n")
          ]

  it "excludes chapters, chat scratch, and a character's sheet/journal from both halves" $ do
    let result = fst <$> runChain (do
          _ <- addAtom "chapters/ch1.md" "Once upon a time...\n"
          _ <- addAtom "chat/scratch.md" "hmm what if...\n"
          _ <- addAtom "sheet.md" "# Alice\n"
          _ <- addAtom "journal.md" "dear diary...\n"
          worldContextOf)
    case result of
      Left err -> expectationFailure err
      Right (WorldLore lore, SystemContext sys) -> do
        lore `shouldBe` []
        sys `shouldBe` []

  it "keeps a nested style.md out of SystemContext -- only the branch root counts" $ do
    isSystemContextPath "notes/style.md" `shouldBe` False
    isSystemContextPath "style.md" `shouldBe` True

  it "sorts multiple lore entries alphabetically" $ do
    let result = fst <$> runChain (do
          _ <- addAtom "notes/zebra.md" "z\n"
          _ <- addAtom "notes/alpha.md" "a\n"
          worldContextOf)
    case result of
      Left err -> expectationFailure err
      Right (WorldLore lore, _) -> case map (\(ContextBlock t) -> t) lore of
        [a, z] -> do
          a `shouldSatisfy` T.isInfixOf "alpha"
          z `shouldSatisfy` T.isInfixOf "zebra"
        other -> expectationFailure ("expected exactly two lore blocks, got " <> show other)

  it "orders beat sheets numerically (library order), not lexically -- ch2 before ch10" $ do
    let result = fst <$> runChain (do
          _ <- addAtom "chapters/ch10.outline.md" "beat ten\n"
          _ <- addAtom "chapters/ch2.outline.md" "beat two\n"
          worldContextOf)
    case result of
      Left err -> expectationFailure err
      Right (WorldLore lore, _) -> case map (\(ContextBlock t) -> t) lore of
        [two, ten] -> do
          two `shouldSatisfy` T.isInfixOf "beat two"
          ten `shouldSatisfy` T.isInfixOf "beat ten"
        other -> expectationFailure ("expected exactly two lore blocks, got " <> show other)

