{-# LANGUAGE OverloadedStrings #-}

-- | 'Storyteller.Writer.Library' is the pure book\/chapter\/scene
-- organizational-tree derivation behind @\/library\/{name}@ (see
-- WS-PROTOCOL.md). Pins:
--
--   * a path is prose (@Unit@) iff some segment of it — an ancestor
--     directory name, or the leaf's own basename stem — contains a marker
--     word (story\/book\/chapter\/scene, singular or plural, or @ch@),
--     wherever in an otherwise arbitrary folder structure it occurs;
--   * @outline.md@ \/ @{stem}.outline.md@ are self-marking, no ancestor
--     marker required;
--   * sibling ordering is natural-sort (@ch2@ before @ch11@), not plain
--     string order, and never attaches stored numeric identity to a node;
--   * every other file/folder still becomes a real, labeled tree node —
--     nothing is filtered out just for not matching a known convention.
module Storyteller.LibrarySpec (spec) where

import Test.Hspec

import Storyteller.Writer.Library

spec :: Spec
spec = do
  describe "classifyPath" $ do
    it "recognizes a flat chapter file" $
      classifyPath "chapters/ch1.md" `shouldBe` Unit

    it "recognizes a chapter's beat sheet" $
      classifyPath "chapters/ch3.outline.md" `shouldBe` UnitOutline

    it "recognizes a bare outline.md as self-marking, no ancestor marker needed" $ do
      classifyPath "outline.md" `shouldBe` UnitOutline
      classifyPath "meta/outline.md" `shouldBe` UnitOutline

    it "recognizes a whole book as one flat, freely-named file" $
      classifyPath "01 - the first book.md" `shouldBe` Unit

    it "recognizes a chapter buried in an arbitrarily deep, freely-named tree" $
      classifyPath "books/01 - the first book/arc1/chapters/chapter 1 - the awakening/story.md"
        `shouldBe` Unit

    it "recognizes a per-chapter-folder outline via the reserved outline.md name" $
      classifyPath "chapters/ch1/outline.md" `shouldBe` UnitOutline

    it "does not recognize a file with no marker word anywhere on its path" $
      classifyPath "notes/misc.md" `shouldBe` OtherFile

    it "does not recognize a marker-free name even without any folder at all" $
      classifyPath "notes.md" `shouldBe` OtherFile

    it "a marker-word ancestor folder (not just the leaf itself) is enough to mark its contents" $
      classifyPath "chapters/notes.md" `shouldBe` Unit

    it "falls back to OtherFile for anything else" $
      classifyPath "characters/alice.md" `shouldBe` OtherFile

  describe "buildLibraryTree" $ do
    it "groups chapter files under a synthesized chapters/ folder node" $ do
      let tree = buildLibraryTree ["chapters/ch1.md", "chapters/ch2.md"]
      map lnKind tree `shouldBe` [Folder]
      map lnPath tree `shouldBe` ["chapters"]
      map lnKind (lnChildren (head tree)) `shouldBe` [Unit, Unit]

    it "sorts naturally, not by plain string order (ch2 before ch11)" $ do
      let tree = buildLibraryTree ["chapters/ch11.md", "chapters/ch2.md", "chapters/ch1.md"]
          chapters = lnChildren (head tree)
      map lnPath chapters `shouldBe` ["chapters/ch1.md", "chapters/ch2.md", "chapters/ch11.md"]

    it "sorts a numeric-prefixed free-text name naturally too" $ do
      let tree = buildLibraryTree ["14 - the finale.md", "2 - the sequel.md", "1 - the beginning.md"]
      map lnPath tree `shouldBe` ["1 - the beginning.md", "2 - the sequel.md", "14 - the finale.md"]

    it "keeps a top-level file as its own root node" $ do
      let tree = buildLibraryTree ["outline.md"]
      map (\n -> (lnPath n, lnKind n)) tree `shouldBe` [("outline.md", UnitOutline)]

    it "does not filter out unrecognized files or folders" $ do
      let tree = buildLibraryTree ["notes/misc.md"]
      map lnKind tree `shouldBe` [Folder]
      map lnKind (lnChildren (head tree)) `shouldBe` [OtherFile]

    it "supports arbitrary, freeform nesting depth for chapters" $ do
      let tree = buildLibraryTree ["series/epic/book3/chapters/ch1.md"]
          descend n = case lnChildren n of
            [child] -> descend child
            []      -> n
            _       -> error "unexpected branching in a single-path test tree"
      lnKind (descend (head tree)) `shouldBe` Unit

    it "every node's own path is populated, not just leaves" $ do
      let tree = buildLibraryTree ["a/b/scene1.md"]
      lnPath (head tree) `shouldBe` "a"
      lnPath (head (lnChildren (head tree))) `shouldBe` "a/b"

  describe "narrativeUnits" $ do
    it "pairs a chapter with its own beat sheet by shared parent directory" $ do
      let tree = buildLibraryTree ["chapters/ch1.md", "chapters/ch1.outline.md"]
          [u] = narrativeUnits tree
      uiPath u `shouldBe` Just "chapters/ch1.md"
      uiOutlinePath u `shouldBe` Just "chapters/ch1.outline.md"

    it "pairs a per-chapter-folder's story.md with its sibling outline.md" $ do
      let tree = buildLibraryTree ["chapters/ch1/story.md", "chapters/ch1/outline.md"]
          [u] = narrativeUnits tree
      uiPath u `shouldBe` Just "chapters/ch1/story.md"
      uiOutlinePath u `shouldBe` Just "chapters/ch1/outline.md"

    it "a chapter with no beat sheet is still its own unit" $ do
      let tree = buildLibraryTree ["chapters/ch1.md"]
          [u] = narrativeUnits tree
      uiOutlinePath u `shouldBe` Nothing

    it "a beat sheet with no prose yet is still its own unit" $ do
      let tree = buildLibraryTree ["chapters/ch3.outline.md"]
          [u] = narrativeUnits tree
      uiPath u `shouldBe` Nothing
      uiOutlinePath u `shouldBe` Just "chapters/ch3.outline.md"

    it "orders units naturally, not by discovery order or plain string order" $ do
      let tree = buildLibraryTree ["chapters/ch11.md", "chapters/ch2.md", "chapters/ch1.md"]
      map uiPath (narrativeUnits tree)
        `shouldBe` [Just "chapters/ch1.md", Just "chapters/ch2.md", Just "chapters/ch11.md"]

    it "recognizes chapters/outlines regardless of surrounding folder nesting" $ do
      let tree = buildLibraryTree ["series/epic/chapters/ch1.md", "series/epic/chapters/ch1.outline.md"]
          [u] = narrativeUnits tree
      uiPath u `shouldBe` Just "series/epic/chapters/ch1.md"
      uiOutlinePath u `shouldBe` Just "series/epic/chapters/ch1.outline.md"

    it "sorts two equally valid naming styles naturally, side by side" $ do
      -- Two obvious ways to name a numbered book: a per-book folder
      -- (numbered in the folder name, "books" itself carrying the marker)
      -- and a flat file (numbered and marked inline, "book 2 - ..."). Both
      -- are recognized and naturally ordered identically to any other pair
      -- of siblings -- no special-casing either shape.
      let tree = buildLibraryTree ["books/1 - the first saga/story.md", "book 2 - the second saga.md"]
      map uiPath (narrativeUnits tree)
        `shouldBe` [Just "book 2 - the second saga.md", Just "books/1 - the first saga/story.md"]
