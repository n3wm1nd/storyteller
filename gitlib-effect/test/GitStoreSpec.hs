{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Correctness check for the direct loose-object write path
-- ('Runix.Git.Store', wired into 'Runix.Git.runGitIO's 'WriteObject'\/
-- 'WriteCommit' handlers) that replaced shelling out to @git
-- hash-object@\/@git mktree@ per write — see that module's doc for why
-- (subprocess-spawn count, not Polysemy, was the real cost a deep
-- tail-replay paid).
--
-- 'GitIOSpec' already round-trips simple, known-good tree/commit/blob
-- shapes through the real interpreter. This adds the case that's actually
-- easy to get wrong when hand-rolling git's tree binary format: entry
-- *sort order*, specifically the directory-vs-file tie-break that a plain
-- byte-wise sort of bare names gets wrong (see 'Runix.Git.serializeTree's
-- own doc). Checked against @git mktree@ itself as the oracle, the same
-- pattern 'GitHashSpec' uses 'GitCliHash'/@git hash-object@ for.
module GitStoreSpec (spec) where

import Control.Concurrent (forkIO)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO (hClose, hSetBinaryMode)
import System.Process
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Polysemy
import Polysemy.Fail (Fail, runFail)
import Polysemy.Resource (Resource, runResource)

import Runix.Cmd (Cmd, Cmds, cmdsIO, interpretCmd)
import Runix.Git
import TestTempRepo (withTempRepo)

runInRepo :: FilePath -> Sem '[Git, Cmd "git", Cmds, Resource, Fail, Embed IO] a -> IO a
runInRepo repo action = do
  result <- runM . runFail . runResource . cmdsIO . interpretCmd @"git" . runGitIOPerCall repo $ action
  either (\e -> ioError (userError ("runGitIO: " <> e))) return result

-- | A tree-entry name distinct enough from others in the same list not to
--   collide, and free of characters that would confuse either git's tree
--   format or the shell-out oracle below.
newtype EntryName = EntryName String deriving (Show, Eq)

instance Arbitrary EntryName where
  arbitrary = EntryName <$> do
    len <- choose (1, 8)
    (:) <$> elements ['a'..'z'] <*> vectorOf (len - 1) (elements (['a'..'z'] ++ ['0'..'9'] ++ "-_"))

data Entry = EntryBlob EntryName | EntrySubTree EntryName deriving Show

instance Arbitrary Entry where
  arbitrary = oneof [EntryBlob <$> arbitrary, EntrySubTree <$> arbitrary]

-- | Build a real 'TreeEntry' list from arbitrary (de-duplicated by name)
--   entries, giving each a synthetic (but valid, 40-hex-char) hash — 'git
--   mktree' never validates that a referenced hash actually resolves to
--   an object, so these don't need to be real.
toTreeEntries :: [Entry] -> [TreeEntry]
toTreeEntries es = go 0 (dedupeByName es)
  where
    go _ [] = []
    go n (EntryBlob (EntryName nm) : rest)    = BlobEntry nm (syntheticHash n) : go (n + 1) rest
    go n (EntrySubTree (EntryName nm) : rest) = SubTree   nm (syntheticHash n) : go (n + 1) rest

    dedupeByName = List.nubBy (\a b -> entryNameOf a == entryNameOf b)
    entryNameOf (EntryBlob (EntryName nm))    = nm
    entryNameOf (EntrySubTree (EntryName nm)) = nm

    syntheticHash n =
      let s = show (n :: Int)
      in ObjectHash (T.pack (s <> replicate (40 - length s) 'a'))

-- | Independently compute the tree hash via a real @git mktree@ call —
--   the same ls-tree-text-in\/hash-out shape the old (pre-'Runix.Git.Store')
--   implementation itself sent to git, so this is exactly the oracle that
--   implementation was already trusted against.
mktreeOracle :: FilePath -> [TreeEntry] -> IO ObjectHash
mktreeOracle repo entries = do
  let entryLine e = case e of
        BlobEntry name h -> "100644 blob " <> unObjectHash h <> "\t" <> T.pack name
        SubTree   name h -> "040000 tree " <> unObjectHash h <> "\t" <> T.pack name
      input = TE.encodeUtf8 (T.unlines (map entryLine entries))
  -- @--missing@: the synthetic hashes these tests use for entry targets
  -- don't correspond to real objects (irrelevant to what's under test —
  -- tree serialisation and sort order, not referential integrity), and
  -- @mktree@ otherwise refuses to build a tree over an unresolvable entry.
  (Just hin, Just hout, _, ph) <- createProcess (proc "git" ["mktree", "--missing"])
    { cwd = Just repo, std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit }
  hSetBinaryMode hin True
  hSetBinaryMode hout True
  _ <- forkIO (BS.hPut hin input >> hClose hin)
  out <- BS.hGetContents hout
  _ <- waitForProcess ph
  return (ObjectHash (T.strip (TE.decodeUtf8 out)))

spec :: Spec
spec = describe "Runix.Git.Store (direct loose-object writes)" $ do
  it "a simple two-entry tree matches git mktree's hash" $
    withTempRepo $ \repo -> do
      let entries = [BlobEntry "a.md" (ObjectHash (T.replicate 40 "a")), SubTree "sub" (ObjectHash (T.replicate 40 "b"))]
      written  <- runInRepo repo (writeTree entries)
      expected <- mktreeOracle repo entries
      written `shouldBe` expected

  prop "matches git mktree for arbitrary distinct entries, including directory/file sort tie-breaks" $
    forAll (listOf1 arbitrary) $ \(es :: [Entry]) ->
      not (null (toTreeEntries es)) ==>
        ioProperty $ withTempRepo $ \repo -> do
          let entries = toTreeEntries es
          written  <- runInRepo repo (writeTree entries)
          expected <- mktreeOracle repo entries
          pure (written == expected)

  it "a directly-written tree is a real, well-formed object git itself recognises" $
    withTempRepo $ \repo -> do
      let entries = [BlobEntry "z.md" (ObjectHash (T.replicate 40 "a")), SubTree "a-dir" (ObjectHash (T.replicate 40 "b"))]
      written <- runInRepo repo (writeTree entries)
      out <- readProcess "git" ["-C", repo, "cat-file", "-t", T.unpack (unObjectHash written)] ""
      out `shouldBe` "tree\n"
