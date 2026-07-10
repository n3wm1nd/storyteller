{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Round-trip tests for 'Storyteller.Writer.Types.CharacterAnswer' --
-- specifically its question\/answer encoding, since both are free-form
-- text that can legitimately contain embedded newlines (a multi-line
-- question, a multi-paragraph answer), unlike 'caCharacter'\/'caFile',
-- which are safe to carry as ordinary 'Storage.Tick' fields.
module Storyteller.Writer.CharacterAnswerSpec (spec) where

import Test.Hspec

import Storage.MockStore (runChain)
import Storage.Tick (storeAs, getTypesTick)

import Storyteller.Core.Types (BranchName(..), fromTick)
import Storyteller.Writer.Types (Character(..), CharacterAnswer(..))

alice :: Character
alice = Character (BranchName "character/alice")

roundTrip :: CharacterAnswer -> Either String (Maybe CharacterAnswer)
roundTrip ca = fst <$> runChain (do
  _ <- storeAs ca
  t <- getTypesTick
  return (fromTick @CharacterAnswer t))

spec :: Spec
spec = describe "CharacterAnswer round-trip" $ do

  it "round-trips a plain single-line question and answer" $ do
    let ca = CharacterAnswer alice "What do you think of Bob?" "He's fine." (Just "scene.md")
    roundTrip ca `shouldBe` Right (Just ca)

  it "round-trips a question containing an embedded newline" $ do
    let ca = CharacterAnswer alice "What do you think of Bob?\nAnd of Carol?" "They're fine." (Just "scene.md")
    roundTrip ca `shouldBe` Right (Just ca)

  it "round-trips a question containing a blank line (two consecutive newlines)" $ do
    let ca = CharacterAnswer alice "What do you think of Bob?\n\nAnd of Carol?" "They're fine." (Just "scene.md")
    roundTrip ca `shouldBe` Right (Just ca)

  it "round-trips a multi-paragraph answer alongside a multi-line question" $ do
    let ca = CharacterAnswer alice
               "Tell me everything.\n\nStart from the beginning."
               "Well.\n\nIt started on a Tuesday.\n\nOr was it Wednesday?"
               (Just "scene.md")
    roundTrip ca `shouldBe` Right (Just ca)

  it "round-trips with no originating file" $ do
    let ca = CharacterAnswer alice "Are you there?" "Always." Nothing
    roundTrip ca `shouldBe` Right (Just ca)
