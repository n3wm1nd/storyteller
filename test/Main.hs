module Main where

import Test.Hspec
import qualified Storyteller.StorageSpec
import qualified Storyteller.SplitDiffMergeSpec
import qualified Storyteller.SplitterSpec
import qualified Storyteller.TrackerSpec
import qualified Storyteller.CharGenSpec

main :: IO ()
main = hspec $ do
  describe "Storyteller.Storage"        Storyteller.StorageSpec.spec
  describe "Storyteller.SplitDiffMerge" Storyteller.SplitDiffMergeSpec.spec
  describe "Storyteller.Splitter"       Storyteller.SplitterSpec.spec
  describe "Storyteller.Tracker"        Storyteller.TrackerSpec.spec
  describe "Storyteller.CharGen"        Storyteller.CharGenSpec.spec
