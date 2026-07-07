{-# LANGUAGE OverloadedStrings #-}

-- | Quick sanity tests for "Storage.Core", against 'Storage.MockStore'
--   -- just enough to check the core primitives (store, drop, at,
--   readAt, reset, inWorktree, and the file operations) aren't
--   completely off-target before the real
--   "Storyteller.Core.StorageMonad" test suite gets ported onto this
--   module.
module Storage.CoreSpec (spec) where

import Prelude hiding (drop, readFile, writeFile)

import Control.Monad.State.Strict (lift)

import Test.Hspec

import Storage.Core
import Storage.MockStore

spec :: Spec
spec = do
  describe "store / drop round trip" $ do
    it "storing a NonAtom then dropping it returns the same tick" $ do
      let result = fst <$> runChain (store (NonAtom [] "type:note\nhello") >> drop)
      result `shouldBe` Right (NonAtom [] "type:note\nhello")

    it "storing an Atom then dropping it returns its path and content" $ do
      let result = fst <$> runChain (store (Atom [] "scene.md" "p1\n") >> drop)
      result `shouldBe` Right (Atom [] "scene.md" "p1\n")

    it "store =<< drop rebuilds the same committed content" $ do
      let result = fst <$> runChain (do
            _ <- store (Atom [] "scene.md" "p1\n")
            t <- drop
            _ <- store t
            committedContent "scene.md")
      result `shouldBe` Right "p1\n"

  describe "atoms build up file content" $ do
    it "two atoms on the same path both land in the committed tree" $ do
      let result = fst <$> runChain (do
            _ <- store (Atom [] "scene.md" "p1\n")
            _ <- store (Atom [] "scene.md" "p2\n")
            committedContent "scene.md")
      result `shouldBe` Right "p1\np2\n"

    it "a NonAtom in between doesn't disturb the atom chain" $ do
      let result = fst <$> runChain (do
            _ <- store (Atom [] "scene.md" "p1\n")
            _ <- store (NonAtom [] "type:note\na note")
            _ <- store (Atom [] "scene.md" "p2\n")
            committedContent "scene.md")
      result `shouldBe` Right "p1\np2\n"

  describe "at" $ do
    it "at edits a tick and replays the tail on top" $ do
      let result = fst <$> runChain (do
            t1 <- store (Atom [] "scene.md" "p1\n")
            _  <- store (Atom [] "scene.md" "p2\n")
            _  <- at t1 $ do
              _ <- drop
              store (Atom [] "scene.md" "p1-revised\n")
            committedContent "scene.md")
      result `shouldBe` Right "p1-revised\np2\n"

    it "at remaps a cross-reference to the target itself" $ do
      let result = runChain $ do
            t1    <- store (Atom [] "scene.md" "p1\n")
            _     <- store (Atom [] "other.md" "unrelated\n")
            _     <- store (NonAtom [t1] "type:note\nabout t1")
            newT1 <- at t1 $ do
              _ <- drop
              store (Atom [] "scene.md" "p1-revised\n")
            -- the note is the last tick before the rebase, so it's the
            -- new head once 'at' replays it back on top; its own ref
            -- should now point at t1's new id, not the old, stale one.
            newHead     <- headHash
            newHeadTick <- lift (readTick newHead)
            return (tickRefs newHeadTick, newT1)
      case result of
        Left err -> expectationFailure err
        Right ((refs, newT1), _finalState) -> refs `shouldBe` [newT1]

    it "at remaps a cross-reference between two tail ticks (not the target itself)" $ do
      let result = runChain $ do
            t1 <- store (Atom [] "scene.md" "p1\n")
            t2 <- store (Atom [] "scene.md" "p2\n")
            _  <- store (NonAtom [t2] "type:note\nabout t2")
            _  <- at t1 $ do
              _ <- drop
              store (Atom [] "scene.md" "p1-revised\n")
            newHead     <- headHash
            newHeadTick <- lift (readTick newHead)
            case tickRefs newHeadTick of
              [refId] -> do
                refTick <- lift (readTick refId)
                return (refId /= t2, refTick)
              other -> fail ("expected exactly one ref, got " <> show (length other))
      case result of
        Left err -> expectationFailure err
        Right ((refChanged, refTick), _finalState) -> do
          refChanged `shouldBe` True
          refTick    `shouldBe` Atom [] "scene.md" "p2\n"

    it "readAt leaves the chain untouched" $ do
      let result = runChain $ do
            t1 <- store (Atom [] "scene.md" "p1\n")
            headBefore <- headHash
            readHead   <- readAt t1 headHash
            headAfter  <- headHash
            return (readHead == t1, headBefore == headAfter)
      case result of
        Left err -> expectationFailure err
        Right ((sawTarget, unchanged), _finalState) -> do
          sawTarget `shouldBe` True
          unchanged `shouldBe` True

    it "readAt discards any store/drop the action performs, however deep" $ do
      let result = runChain $ do
            t1 <- store (Atom [] "scene.md" "p1\n")
            headBefore <- headHash
            _ <- readAt t1 $ do
              _ <- drop
              _ <- store (Atom [] "scene.md" "should not stick\n")
              store (NonAtom [] "type:note\nalso should not stick")
            headAfter <- headHash
            content   <- committedContent "scene.md"
            return (headBefore == headAfter, content)
      case result of
        Left err -> expectationFailure err
        Right ((unchanged, content), _finalState) -> do
          unchanged `shouldBe` True
          content   `shouldBe` "p1\n"

    it "at fails when target isn't in head's history" $ do
      let result = runChain $ at (ObjectHash "nonexistent") (return ())
      case result of
        Left err -> err `shouldContain` "not found in history"
        Right _  -> expectationFailure "expected at to fail on an unknown target"

  describe "reset / inWorktree" $ do
    it "reset doesn't move head" $ do
      let result = fst <$> runChain (do
            _      <- store (Atom [] "scene.md" "p1\n")
            before <- headHash
            reset
            after  <- headHash
            return (before == after))
      result `shouldBe` Right True

    it "inWorktree doesn't shield a store's effect on head -- only the ambient tree is its concern, not the chain" $ do
      let result = fst <$> runChain (do
            _      <- store (Atom [] "scene.md" "p1\n")
            before <- headHash
            _      <- inWorktree (store (Atom [] "scene.md" "p2\n"))
            after  <- headHash
            return (before /= after))
      result `shouldBe` Right True

    it "inWorktree restores whatever the ambient tree held before, once the action returns" $ do
      let result = fst <$> runChain (do
            _ <- store (Atom [] "scene.md" "p1\n")
            writeFile "scratch.md" "pending draft"
            _       <- inWorktree (readFile "scene.md")
            content <- readFile "scratch.md"
            return content)
      result `shouldBe` Right "pending draft"

    it "still returns the inner action's own value" $ do
      let result = fst <$> runChain (inWorktree (return (42 :: Int)))
      result `shouldBe` Right 42

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

  describe "NonAtom fallback" $ do
    it "a message with no atom shape at all decodes as a NonAtom, verbatim" $ do
      let result = fst <$> runChain (do
            _ <- store (NonAtom [] "type:prompt\nwrite more")
            drop)
      result `shouldBe` Right (NonAtom [] "type:prompt\nwrite more")

    it "fields present but no atom tag still falls back to NonAtom" $ do
      let result = runMockGit $ do
            emptyTreeHash <- writeObject (TreeObject [])
            h <- writeCommit CommitData
              { commitParents = []
              , commitTree    = emptyTreeHash
              , commitMessage = "file:scene.md\n\ntype:note\nnot an atom"
              }
            readTick h
      result `shouldBe` Right (NonAtom [] "file:scene.md\n\ntype:note\nnot an atom")
