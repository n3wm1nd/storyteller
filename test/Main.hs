module Main where

import Test.Hspec
import qualified Storyteller.StorageSpec

main :: IO ()
main = hspec $ do
  describe "Storyteller.Storage" Storyteller.StorageSpec.spec
