{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

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

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail)

import Storyteller.Core.Context (ContextRow, ContextStorage, runContextValue)
import Runix.FileSystem (FileSystem, FileSystemRead)
import Storyteller.Core.Branch (Branches)
import Storyteller.Core.Git (BranchOp, runBranchAndFS)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Context.DSL.Compile (ContextFS, emptyLibrary, runDefinition)
import Storyteller.Context.DSL.Parser (parseDefinition, renderParseErr)
import Storyteller.Context.DSL.QQ (dsl)
import Storyteller.Context.DSL.Value

injuryStatus :: forall branch r. Members '[FileSystem ContextFS, FileSystemRead ContextFS,Fail] r => Action r (Value r)
injuryStatus = [dsl|
as "injury": read status/injury.md
|]

-- | The 'injury' export's own text, or a sentinel if the definition
--   somehow stopped exporting one at all -- what both specs below force.
injuryText :: Value r -> Action r T.Text
injuryText v = case lookup "injury" (valueEntries v) of
  Nothing     -> pure "no 'injury' export"
  Just action -> messagesText <$> (valueDefault =<< action)

runDslOn
  :: forall a
  .  BranchName
  -> (forall r. Members '[BranchOp Main, Branches, ContextStorage, Fail] r => Action (ContextRow r) a)
  -> Sem (StoryStorage : TestEffects '[]) a
runDslOn bname act = runBranchAndFS @Main bname (runContextValue @Main act)

spec :: Spec
spec = describe "[dsl| ... |]" $ do
  it "behaves like any other Definition: absence, not an error, for a file that doesn't exist" $
    run (testStack $ do
      _ <- createBranch (BranchName "empty")
      runDslOn (BranchName "empty") (injuryStatus @Main >>= injuryText))
      `shouldBe` Right "" -- no status/injury.md in an empty scope -> absence, not an error -> empty text

  it "produces the same result as parseDefinition + runDefinition on identical text" $
    run (testStack $ do
      _ <- createBranch (BranchName "empty")
      runDslOn (BranchName "empty") (injuryStatus @Main >>= injuryText))
      `shouldBe`
      run (testStack $ do
        _ <- createBranch (BranchName "empty")
        runDslOn (BranchName "empty") (manualDsl @Main >>= injuryText))
  where
    manualDsl :: forall branch r. Members '[FileSystem ContextFS, FileSystemRead ContextFS,Fail] r => Action r (Value r)
    manualDsl = case parseDefinition "<test>" (T.unlines ["as \"injury\": read status/injury.md"]) of
      Left err  -> fail (T.unpack (renderParseErr err))
      Right def -> runDefinition emptyLibrary def []
