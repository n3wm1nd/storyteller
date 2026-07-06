module Main (main) where

import Test.Hspec
import qualified GitHashSpec
import qualified GitIOSpec
import qualified GitBatchSpec
import qualified GitStoreSpec

main :: IO ()
main = hspec $ do
  describe "Runix.Git.Hash" GitHashSpec.spec
  describe "Runix.Git" GitIOSpec.spec
  describe "Runix.Git.Batch" GitBatchSpec.spec
  describe "Runix.Git.Store" GitStoreSpec.spec
