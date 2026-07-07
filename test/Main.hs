module Main where

import Test.Hspec
import qualified Storage.CoreSpec
import qualified Storage.FSSpec
import qualified Storage.OpsSpec
import qualified Storage.CommitWorktreeSpec
import qualified Storage.ChainEditSpec
import qualified Storage.TickSpec
import qualified Storyteller.FileAtomsSpec
import qualified Storyteller.AtGenericSpec
import qualified Storyteller.StorageSpec
import qualified Storyteller.GitCascadeSpec
import qualified Storyteller.AppendSpec
import qualified Storyteller.CommitNewFilesSpec
import qualified Storyteller.CreateSpec
import qualified Storyteller.SubdirSpec
import qualified Storyteller.SplitterSpec
import qualified Storyteller.TrackerSpec
import qualified Storyteller.CharGenSpec
import qualified Storyteller.PresenceSpec
import qualified Storyteller.UndoSpec
import qualified Storyteller.ChatSpec
import qualified Server.BranchSpec
import qualified Server.Writer.BranchSpec
import qualified Server.FileSpec
import qualified Server.CharacterSpec
import qualified Server.NotificationSpec
import qualified Server.Writer.GitWorkerSpec
import Server.TestStack (testStack, testStackTransactional)

main :: IO ()
main = hspec $ do
  describe "Storage.Core"               Storage.CoreSpec.spec
  describe "Storage.FS"                Storage.FSSpec.spec
  describe "Storage.Ops"                Storage.OpsSpec.spec
  describe "Storage.CommitWorktree"     Storage.CommitWorktreeSpec.spec
  describe "Storage.ChainEdit"          Storage.ChainEditSpec.spec
  describe "Storage.Tick"               Storage.TickSpec.spec
  describe "Storyteller.FileAtoms"      Storyteller.FileAtomsSpec.spec
  describe "Storyteller.AtGeneric"     Storyteller.AtGenericSpec.spec
  describe "Storyteller.Core.Storage"        Storyteller.StorageSpec.spec
  describe "Storyteller.Core.Git (cascadeReplace)" Storyteller.GitCascadeSpec.spec
  describe "Storyteller.Core.Append"         Storyteller.AppendSpec.spec
  describe "Storyteller.CommitNewFiles" Storyteller.CommitNewFilesSpec.spec
  describe "Storyteller.Create"         Storyteller.CreateSpec.spec
  describe "Storyteller.Subdir"         Storyteller.SubdirSpec.spec
  describe "Storyteller.Splitter"       Storyteller.SplitterSpec.spec
  describe "Storyteller.Tracker"        Storyteller.TrackerSpec.spec
  describe "Storyteller.CharGen"        Storyteller.CharGenSpec.spec
  describe "Storyteller.Presence"       Storyteller.PresenceSpec.spec
  describe "Storyteller.Undo"           Storyteller.UndoSpec.spec
  describe "Storyteller.Writer.Agent.Chat" Storyteller.ChatSpec.spec
  -- Server.Core.Branch/Server.Core.File are written once against
  -- 'TestRunner' (see Server.TestStack) and run under both interpreters:
  -- eager, and buffered through 'Storyteller.Core.Git.withStorage' — the
  -- transaction wrapping every real server command actually runs under. A
  -- bug once slipped past the whole suite by only showing up through the
  -- buffered path; running both here is what closes that gap.
  describe "Server.Core.Branch (eager)"         (Server.BranchSpec.spec testStack)
  describe "Server.Core.Branch (withStorage)"   (Server.BranchSpec.spec testStackTransactional)
  describe "Server.Writer.Branch (eager)"       (Server.Writer.BranchSpec.spec testStack)
  describe "Server.Writer.Branch (withStorage)" (Server.Writer.BranchSpec.spec testStackTransactional)
  describe "Server.Core.File (eager)"           (Server.FileSpec.spec testStack)
  describe "Server.Core.File (withStorage)"     (Server.FileSpec.spec testStackTransactional)
  describe "Server.Writer.Character"            Server.CharacterSpec.spec
  describe "Server.Writer.Notification"         Server.NotificationSpec.spec
  describe "Server.Writer.GitWorker"            Server.Writer.GitWorkerSpec.spec
