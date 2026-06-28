module Main where

import Test.Hspec
import qualified Storyteller.StorageSpec
import qualified Storyteller.SplitDiffMergeSpec

main :: IO ()
main = hspec $ do
  describe "Storyteller.Storage"       Storyteller.StorageSpec.spec
  describe "Storyteller.SplitDiffMerge" Storyteller.SplitDiffMergeSpec.spec
