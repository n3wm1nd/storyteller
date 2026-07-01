module Main where

import Test.Hspec
import qualified Storyteller.FileAtomsSpec
import qualified Storyteller.StorageSpec
import qualified Storyteller.EditSpec
import qualified Storyteller.CommitWorkingTreeSpec
import qualified Storyteller.SplitterSpec
import qualified Storyteller.TrackerSpec
import qualified Storyteller.CharGenSpec
import qualified Server.BranchSpec
import qualified Server.FileSpec
import qualified Server.NotificationSpec

main :: IO ()
main = hspec $ do
  describe "Storyteller.FileAtoms"      Storyteller.FileAtomsSpec.spec
  describe "Storyteller.Storage"        Storyteller.StorageSpec.spec
  describe "Storyteller.Edit"           Storyteller.EditSpec.spec
  describe "Storyteller.CommitWorkingTree" Storyteller.CommitWorkingTreeSpec.spec
  describe "Storyteller.Splitter"       Storyteller.SplitterSpec.spec
  describe "Storyteller.Tracker"        Storyteller.TrackerSpec.spec
  describe "Storyteller.CharGen"        Storyteller.CharGenSpec.spec
  describe "Server.Branch"              Server.BranchSpec.spec
  describe "Server.File"                Server.FileSpec.spec
  describe "Server.Notification"        Server.NotificationSpec.spec
