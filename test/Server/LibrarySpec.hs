{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | 'Server.Writer.Library.libraryTree' folds chapter headings via
--   'Storage.Core.memoFold' instead of re-reading every chapter's content
--   on every call — these tests pin the same two properties
--   'Storage.CoreSpec'\'s own 'memoFold' tests do, but through the real
--   composition ('libraryTree' itself, not the raw primitive): a cache
--   thread across calls gives the same answer a cold call would, and a
--   supplied cache is actually *trusted* (short-circuiting the fold), not
--   just harmless to pass in.
module Server.LibrarySpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec

import Polysemy (Sem, run)

import Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite)
import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Core.Git (BranchTag, BranchOp, runBranchAndFS, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch)
import Storyteller.Core.Types (BranchName(..))

import Server.Core.Branch (Main)
import Server.Writer.Library (libraryTree, chapterCreate)
import Storyteller.Writer.Library (LibraryNode(..), LibraryKind(..))
import Server.TestStack

withLibraryBranch
  :: T.Text
  -> Sem ( FileSystemWrite (BranchTag Main)
         : FileSystemRead  (BranchTag Main)
         : FileSystem      (BranchTag Main)
         : BranchOp Main
         : StoryStorage
         : TestEffects '[] ) a
  -> Either String a
withLibraryBranch name action = run $ testStack $ do
  _ <- createBranch (BranchName name)
  runBranchAndFS @Main (BranchName name) action

-- | Every chapter-kind leaf, in the tree's own order.
collectChapters :: [LibraryNode] -> [LibraryNode]
collectChapters = concatMap go
  where
    go n = case lnKind n of
      Folder    -> collectChapters (lnChildren n)
      Chapter _ -> [n]
      _         -> []

spec :: Spec
spec = describe "libraryTree" $ do

  it "reports a chapter's own heading" $ do
    let result = withLibraryBranch "story" $ do
          chapterCreate "chapters/ch1.md" "Chapter One"
          (tree, _, _) <- libraryTree []
          return tree
    case result of
      Left err   -> expectationFailure err
      Right tree -> map lnHeading (collectChapters tree) `shouldBe` [Just "# Chapter One"]

  it "reuses a cache threaded from a previous call, giving the same headings a cold call would" $ do
    let result = withLibraryBranch "story" $ do
          chapterCreate "chapters/ch1.md" "Chapter One"
          (_, _, cache1) <- libraryTree []
          chapterCreate "chapters/ch2.md" "Chapter Two"
          (tree2, _, _)  <- libraryTree cache1
          return tree2
    case result of
      Left err   -> expectationFailure err
      Right tree -> map lnHeading (collectChapters tree)
        `shouldBe` [Just "# Chapter One", Just "# Chapter Two"]

  -- The real proof the cache is trusted, not just harmless: a deliberately
  -- *wrong* cached value for chapter one's content must surface verbatim in
  -- its reported heading -- if 'libraryTree' always recomputed from
  -- scratch regardless of what's passed in, the wrong value could never
  -- show up.
  it "trusts a supplied cache instead of recomputing an unchanged chapter" $ do
    let result = withLibraryBranch "story" $ do
          chapterCreate "chapters/ch1.md" "Chapter One"
          afterCh1 <- runStorage @Main Core.headHash
          chapterCreate "chapters/ch2.md" "Chapter Two"
          let wrongCache = [(afterCh1, Map.singleton "chapters/ch1.md" "WRONG\n")]
          (tree, _, _) <- libraryTree wrongCache
          return tree
    case result of
      Left err   -> expectationFailure err
      Right tree -> map lnHeading (collectChapters tree)
        `shouldBe` [Just "WRONG", Just "# Chapter Two"]

  -- The client's own cue not to open a prose/atom viewer on a path (see
  -- the "upload a binary file" design conversation): a never-atom-tracked
  -- path is flagged 'lnBinary', an ordinary chapter is not.
  it "flags an uploaded binary file as lnBinary, and leaves an ordinary chapter unflagged" $ do
    let result = withLibraryBranch "story" $ do
          chapterCreate "chapters/ch1.md" "Chapter One"
          runStorage @Main (Core.writeFile "portrait.png" "\xFF\xFE\x00" >> Ops.commitFiles ["portrait.png"])
          (tree, _, _) <- libraryTree []
          return tree
    case result of
      Left err   -> expectationFailure err
      Right tree -> do
        let byPath p = [ lnBinary n | n <- flatten tree, lnPath n == p ]
            flatten ns = ns ++ concatMap (flatten . lnChildren) ns
        byPath "chapters/ch1.md" `shouldBe` [False]
        byPath "portrait.png"    `shouldBe` [True]
