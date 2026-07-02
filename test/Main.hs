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
import Server.TestStack (testStack, testStackTransactional)

main :: IO ()
main = hspec $ do
  describe "Storyteller.FileAtoms"      Storyteller.FileAtomsSpec.spec
  describe "Storyteller.Storage"        Storyteller.StorageSpec.spec
  describe "Storyteller.Edit"           Storyteller.EditSpec.spec
  describe "Storyteller.CommitWorkingTree" Storyteller.CommitWorkingTreeSpec.spec
  describe "Storyteller.Splitter"       Storyteller.SplitterSpec.spec
  describe "Storyteller.Tracker"        Storyteller.TrackerSpec.spec
  describe "Storyteller.CharGen"        Storyteller.CharGenSpec.spec
  -- Server.Branch/Server.File are written once against 'TestRunner' (see
  -- Server.TestStack) and run under both interpreters: eager, and
  -- buffered through 'Storyteller.Git.withStorage' — the transaction
  -- wrapping every real server command actually runs under. A bug once
  -- slipped past the whole suite by only showing up through the buffered
  -- path; running both here is what closes that gap.
  describe "Server.Branch (eager)"              (Server.BranchSpec.spec testStack)
  describe "Server.Branch (withStorage)"        (Server.BranchSpec.spec testStackTransactional)
  describe "Server.File (eager)"                (Server.FileSpec.spec testStack)
  describe "Server.File (withStorage)"          (Server.FileSpec.spec testStackTransactional)
  describe "Server.Notification"        Server.NotificationSpec.spec
