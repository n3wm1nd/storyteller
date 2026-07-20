{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | @['dsl'| ... |]@ parses at GHC compile time -- this file compiling
--   at all is itself half the test (a malformed definition here would
--   be a build failure, not something these specs could ever run). What
--   remains checkable at hspec-runtime: the embedded 'Definition'
--   matches what 'parseDefinition' produces for identical text, and it
--   runs through 'compileDefinition' the same as any other 'Definition'.
module Storyteller.Context.DSL.QQSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec

import Storage.MockStore (runMockGit)

import Storyteller.Context.DSL.AST (Definition)
import Storyteller.Context.DSL.Compile (coreFilters, compileDefinition)
import Storyteller.Context.DSL.Parser (parseDefinition)
import Storyteller.Context.DSL.QQ (dsl)
import Storyteller.Context.DSL.Value

injuryStatus :: Definition
injuryStatus = [dsl|
as "injury": read status/injury.md
|]

spec :: Spec
spec = describe "[dsl| ... |]" $ do
  it "embeds exactly the Definition parseDefinition would produce for the same text" $
    Right injuryStatus `shouldBe` parseDefinition "<test>"
      (T.unlines ["as \"injury\": read status/injury.md"])

  it "runs through compileDefinition like any other Definition" $
    runMockGit
      (runAction
        (do
          v <- compileDefinition coreFilters injuryStatus emptyValue []
          case Map.lookup "injury" (valueEntries v) of
            Nothing     -> pure "no 'injury' export"
            Just action -> messagesText <$> (valueDefault =<< action))
        (const (pure Nothing)))
      `shouldBe` Right "" -- no status/injury.md in an empty scope -> absence, not an error -> empty text
