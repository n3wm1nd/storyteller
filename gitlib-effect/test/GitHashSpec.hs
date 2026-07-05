{-# LANGUAGE OverloadedStrings #-}

module GitHashSpec (spec) where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Runix.Git (ObjectHash(..))
import Runix.Git.Hash (ObjectKind(..), hashObject)
import GitCliHash (gitHashObject)

instance Arbitrary ObjectKind where
  arbitrary = elements [Blob, Tree, Commit]

-- | Arbitrary object content. Bounded by QuickCheck's own size parameter
-- (so this stays small during normal runs) -- large enough to exercise
-- more than one byte of the length-prefix, small enough that shelling out
-- to git a few hundred times in this spec stays fast.
genContent :: Gen BS.ByteString
genContent = BS.pack <$> (arbitrary :: Gen [Word8])

spec :: Spec
spec = describe "Runix.Git.Hash.hashObject" $ do
  it "matches a known git hash-object result for a blob" $
    ObjectHash (hashObject Blob "hello world")
      `shouldBe` ObjectHash "95d09f2b10159347eece71399a7e2e907ea3df4f"

  it "matches git hash-object for empty content" $ do
    reference <- gitHashObject Blob ""
    ObjectHash (hashObject Blob "") `shouldBe` reference

  prop "matches git hash-object for arbitrary content and object kind" $
    forAll arbitrary $ \kind ->
    forAll genContent $ \content ->
      ioProperty $ do
        reference <- gitHashObject kind content
        pure (ObjectHash (hashObject kind content) == reference)
