{-# LANGUAGE LambdaCase #-}
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
import qualified Data.List

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

  describe "editTick / replaceTick / resolveId" $ do
    it "editTick logs its own replacement, resolvable afterward" $ do
      let result = runChain $ do
            t1  <- store (Atom [] "scene.md" "p1\n")
            new <- editTick $ \case
              Atom refs path content -> return (Atom refs path (content <> "edited\n"))
              other                  -> return other
            resolved <- resolveId t1
            return (new, resolved)
      case result of
        Left err -> expectationFailure err
        Right ((new, resolved), _finalState) -> resolved `shouldBe` new

    it "resolveId is the identity for an id nothing has ever replaced" $ do
      let result = fst <$> runChain (do
            t1 <- store (Atom [] "scene.md" "p1\n")
            resolved <- resolveId t1
            return (resolved == t1))
      result `shouldBe` Right True

    it "a ref captured before an unrelated at, then used by a later at's target, still resolves to where that tick actually ended up" $ do
      -- t1 gets edited (id changes) entirely independently of anything
      -- holding a reference to it; a *later* operation, given only t1's
      -- original (now-stale) id, must still land on the right tick --
      -- this is the case 'at'\/'readAt' resolving their own @target@
      -- exists for, distinct from 'at'\'s own tail-replay (which only
      -- ever sees refs embedded *inside* the ticks it's replaying).
      let result = runChain $ do
            t1     <- store (Atom [] "scene.md" "p1\n")
            _      <- store (Atom [] "other.md" "unrelated\n")
            newT1  <- at t1 $ do
              _ <- drop
              store (Atom [] "scene.md" "p1-revised\n")
            -- now use the *original*, stale t1 id as an 'at' target --
            -- not newT1, which nothing outside this module would know
            -- to use without consulting 'resolveId' itself.
            newerT1 <- at t1 $ do
              _ <- drop
              store (Atom [] "scene.md" "p1-revised-again\n")
            scene   <- committedContent "scene.md"
            other   <- committedContent "other.md"
            return (newT1 /= newerT1, scene, other)
      case result of
        Left err -> expectationFailure err
        Right ((idsDiffer, scene, other), _finalState) -> do
          idsDiffer `shouldBe` True
          scene     `shouldBe` "p1-revised-again\n"
          other     `shouldBe` "unrelated\n"

  describe "follow" $ do
    it "folds over every tick from head back through root, oldest last" $ do
      let result = fst <$> runChain (do
            _ <- store (Atom [] "scene.md" "p1\n")
            _ <- store (NonAtom [] "type:note\na note")
            _ <- store (Atom [] "scene.md" "p2\n")
            follow [] (\acc _h t -> (t : acc, True)))
      result `shouldBe` Right
        [ NonAtom [] "type:root\n"
        , Atom [] "scene.md" "p1\n"
        , NonAtom [] "type:note\na note"
        , Atom [] "scene.md" "p2\n"
        ]

    it "stops early when the step function says so" $ do
      let result = fst <$> runChain (do
            _ <- store (Atom [] "scene.md" "p1\n")
            _ <- store (Atom [] "scene.md" "p2\n")
            _ <- store (Atom [] "scene.md" "p3\n")
            follow (0 :: Int) (\n _h _t -> (n + 1, n < 1)))
      result `shouldBe` Right 2

    it "each tick's own hash matches what re-reading it directly gives" $ do
      let result = runChain $ do
            t1 <- store (Atom [] "scene.md" "p1\n")
            _  <- store (Atom [] "scene.md" "p2\n")
            follow [] (\acc h _t -> ((h == t1) : acc, True))
      case result of
        Left err -> expectationFailure err
        Right (seen, _finalState) -> seen `shouldBe` [False, True, False]

  describe "syncTo" $ do
    it "jumps head to the given hash and resets the ambient tree to match it" $ do
      let result = runChain $ do
            t1 <- store (Atom [] "scene.md" "p1\n")
            _  <- store (Atom [] "scene.md" "p2\n")
            writeFile "scratch.md" "pending"
            syncTo t1
            h       <- headHash
            content <- readFile "scene.md"
            stray   <- elem "scratch.md" <$> list
            return (h == t1, content, stray)
      case result of
        Left err -> expectationFailure err
        Right ((atT1, content, stray), _finalState) -> do
          atT1    `shouldBe` True
          content `shouldBe` "p1\n"
          stray   `shouldBe` False

    it "resolves a stale id before jumping, same as at/readAt" $ do
      let result = runChain $ do
            t1    <- store (Atom [] "scene.md" "p1\n")
            newT1 <- at t1 $ do
              _ <- drop
              store (Atom [] "scene.md" "p1-revised\n")
            syncTo t1  -- stale, pre-edit id
            h <- headHash
            return (h == newT1)
      case result of
        Left err -> expectationFailure err
        Right (atNewT1, _finalState) -> atNewT1 `shouldBe` True

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
