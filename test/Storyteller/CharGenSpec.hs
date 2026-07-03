{-# LANGUAGE OverloadedStrings #-}

module Storyteller.CharGenSpec (spec) where

import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import           Paths_storyteller (getDataFileName)
import           Test.Hspec
import           Test.QuickCheck

import           Storyteller.Writer.Agent.CharGen
  ( charGenAgent, ScenarioTemplate(..), RngSeed(..), CharSheet(..) )

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

loadMinimal :: IO ScenarioTemplate
loadMinimal = do
  path <- getDataFileName "test/fixtures/minimal.yaml"
  ScenarioTemplate <$> Yaml.decodeFileThrow path

run :: ScenarioTemplate -> Int -> T.Text
run tmpl seed = unSheet (charGenAgent tmpl (RngSeed seed))

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = beforeAll loadMinimal $ do

  describe "fixture round-trip" $ do

    it "parses minimal.yaml and produces non-empty output" $ \tmpl ->
      run tmpl 0 `shouldSatisfy` (not . T.null)

  describe "structure" $ do

    it "sheet contains the scenario name" $ \tmpl ->
      run tmpl 0 `shouldSatisfy` T.isInfixOf "MINIMAL"

    it "sheet contains both stat names" $ \tmpl -> do
      let t = run tmpl 0
      t `shouldSatisfy` T.isInfixOf "Strength"
      t `shouldSatisfy` T.isInfixOf "Dexterity"

    it "exactly one background is selected" $ \tmpl ->
      let t = run tmpl 0
      in  (T.isInfixOf "Scholar" t && T.isInfixOf "Soldier" t) `shouldBe` False

    it "exactly one trait is selected" $ \tmpl ->
      let t     = run tmpl 0
          count = length $ filter (`T.isInfixOf` t) ["Brave", "Cautious", "Curious"]
      in  count `shouldBe` 1

  describe "determinism" $ do

    it "same seed produces identical output" $ \tmpl ->
      run tmpl 123 `shouldBe` run tmpl 123

    it "seeds 0–9 do not all produce identical output" $ \tmpl ->
      let outputs = map (run tmpl) [0..9]
      in  length (filter (== head outputs) outputs) `shouldSatisfy` (< 10)

  describe "QuickCheck" $ do

    it "output is always non-empty for any seed" $ \tmpl ->
      property $ \s -> not (T.null (run tmpl s))

    it "output always contains the scenario name" $ \tmpl ->
      property $ \s -> T.isInfixOf "MINIMAL" (run tmpl s)

    it "exactly one background is always selected" $ \tmpl ->
      property $ \s ->
        let t = run tmpl s
        in  T.isInfixOf "Scholar" t /= T.isInfixOf "Soldier" t

    it "exactly one trait is always selected" $ \tmpl ->
      property $ \s ->
        let t     = run tmpl s
            count = length $ filter (`T.isInfixOf` t) ["Brave", "Cautious", "Curious"]
        in  count === 1
