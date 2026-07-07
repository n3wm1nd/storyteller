{-# LANGUAGE OverloadedStrings #-}

-- | Sanity tests for "Storage.FS" -- the ambient-tree directory\/listing
--   operations ('createDirectory'\/'remove'\/'removeRecursive'\/'list'\/
--   'isDirectory'\/'listChildren') and the 'readFile'\/'writeFile' they
--   compose with -- against 'Storage.MockStore'. These lived in
--   "Storage.CoreSpec" before the directory\/listing operations moved out
--   of "Storage.Core" into their own module.
module Storage.FSSpec (spec) where

import Prelude hiding (drop, readFile, writeFile)

import qualified Data.List

import Test.Hspec

import Storage.FS
import Storage.MockStore (runChain)

spec :: Spec
spec = do
  describe "ambient file access" $ do
    it "writeFile then readFile round-trips" $ do
      let result = fst <$> runChain (do
            writeFile "notes.md" "hello"
            readFile "notes.md")
      result `shouldBe` Right "hello"

    it "readFile fails on a path that was never written" $ do
      let result = fst <$> runChain (readFile "missing.md")
      case result of
        Left _  -> return ()
        Right _ -> expectationFailure "expected readFile to fail on a missing path"

    it "remove makes a written file disappear" $ do
      let result = fst <$> runChain (do
            writeFile "notes.md" "hello"
            remove "notes.md"
            readFile "notes.md")
      case result of
        Left _  -> return ()
        Right _ -> expectationFailure "expected readFile to fail after remove"

    it "createDirectory introduces an explicit directory entry" $ do
      let result = fst <$> runChain (do
            createDirectory "chapters"
            readFile "chapters")
      case result of
        Left err -> err `shouldContain` "is a directory"
        Right _  -> expectationFailure "expected readFile on a directory to fail"

    it "writeFile creates ancestor directory entries automatically" $ do
      let result = fst <$> runChain (do
            writeFile "chapters/one.md" "content"
            readFile "chapters")
      case result of
        Left err -> err `shouldContain` "is a directory"
        Right _  -> expectationFailure "expected chapters to register as a directory"

    it "isDirectory is True for an explicit directory and False for a file or unknown path" $ do
      let result = fst <$> runChain (do
            createDirectory "chapters"
            writeFile "notes.md" "hello"
            isDir  <- isDirectory "chapters"
            isFile <- isDirectory "notes.md"
            isMiss <- isDirectory "nowhere"
            return (isDir, isFile, isMiss))
      result `shouldBe` Right (True, False, False)

    it "listChildren returns only the direct children of a directory" $ do
      let result = fst <$> runChain (do
            writeFile "chapters/one.md" "1"
            writeFile "chapters/two.md" "2"
            writeFile "chapters/sub/three.md" "3"
            writeFile "root.md" "r"
            listChildren "chapters")
      case result of
        Left err       -> expectationFailure err
        Right children -> Data.List.sort children `shouldBe` ["chapters/one.md", "chapters/sub", "chapters/two.md"]

    it "listChildren on the ambient root only sees top-level entries" $ do
      let result = fst <$> runChain (do
            writeFile "chapters/one.md" "1"
            writeFile "root.md" "r"
            listChildren "/")
      case result of
        Left err       -> expectationFailure err
        Right children -> Data.List.sort children `shouldBe` ["chapters", "root.md"]
