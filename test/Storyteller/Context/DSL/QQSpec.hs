{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | @['dsl'| ... |]@ parses at GHC compile time -- this file compiling
--   at all is itself half the test (a malformed definition here would
--   be a build failure, not something these specs could ever run). What
--   remains checkable at hspec-runtime: 'injuryStatus' behaves exactly
--   like the DSL's own spec says it should, and produces the same
--   result as calling 'parseDefinition'\/'runDefinition' directly on
--   identical text would -- the quoter contributes no new semantics,
--   only moving *when* parsing happens and giving the result the
--   curried-function shape "Storyteller.Context.DSL.QQ" describes.
module Storyteller.Context.DSL.QQSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Storage.MockStore (Mock, runChain)

import Storyteller.Context.DSL.Compile (runDefinition)
import Storyteller.Context.DSL.Parser (parseDefinition, renderParseErr)
import Storyteller.Context.DSL.QQ (dsl)
import Storyteller.Context.DSL.Value

-- | 'Mock' has no notion of branches at all -- this file is the only
--   place that needs one, so it stays a local orphan instance rather
--   than something "Storage.MockStore" (shared by unrelated specs)
--   has to carry.
instance MonadBranch Mock where
  resolveBranch _ = pure Nothing

injuryStatus :: Action Value
injuryStatus = [dsl|
as "injury": read status/injury.md
|]

-- | The 'injury' export's own text, or a sentinel if the definition
--   somehow stopped exporting one at all -- what both specs below force.
injuryText :: Value -> Action T.Text
injuryText v = case lookup "injury" (valueEntries v) of
  Nothing     -> pure "no 'injury' export"
  Just action -> messagesText <$> (valueDefault =<< action)

spec :: Spec
spec = describe "[dsl| ... |]" $ do
  it "behaves like any other Definition: absence, not an error, for a file that doesn't exist" $
    (fst <$> runChain (runAction (injuryStatus >>= injuryText)))
      `shouldBe` Right "" -- no status/injury.md in an empty scope -> absence, not an error -> empty text

  it "produces the same result as parseDefinition + runDefinition on identical text" $
    (fst <$> runChain (runAction (injuryStatus >>= injuryText)))
      `shouldBe`
      (fst <$> runChain (runAction (manualDsl >>= injuryText)))
  where
    manualDsl :: Action Value
    manualDsl = case parseDefinition "<test>" (T.unlines ["as \"injury\": read status/injury.md"]) of
      Left err  -> fail (T.unpack (renderParseErr err))
      Right def -> runDefinition def []
