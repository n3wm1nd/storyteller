{-# LANGUAGE OverloadedStrings #-}

-- | 'Storyteller.Writer.Library' is the pure book\/chapter\/scene
-- organizational-tree derivation behind @\/library\/{name}@ (see
-- WS-PROTOCOL.md). Pins:
--
--   * @chapters\/ch{N}.md@\/@chapters\/ch{N}.outline.md@\/@outline.md@ are
--     detected purely from basename + immediate parent dirname, wherever in
--     an otherwise arbitrary folder structure they occur — the "freeform
--     depth" requirement this was deliberately designed around;
--   * chapters sort numerically (@ch2@ before @ch10@), not alphabetically;
--   * every other file/folder still becomes a real, labeled tree node —
--     nothing is filtered out just for not matching a known convention.
module Storyteller.LibrarySpec (spec) where

import Test.Hspec

import Storyteller.Writer.Library

spec :: Spec
spec = do
  describe "classifyPath" $ do
    it "recognizes a chapter file" $
      classifyPath "chapters/ch1.md" `shouldBe` Chapter 1

    it "recognizes a chapter's beat sheet" $
      classifyPath "chapters/ch3.outline.md" `shouldBe` ChapterOutline 3

    it "recognizes the story outline anywhere, not just at the root" $ do
      classifyPath "outline.md" `shouldBe` StoryOutline
      classifyPath "meta/outline.md" `shouldBe` StoryOutline

    it "recognizes a chapter arbitrarily deep in a user-chosen folder structure" $
      classifyPath "series/epic/book3/act1/chapters/ch1.md" `shouldBe` Chapter 1

    it "does not recognize a chapters/ file that doesn't match the ch{N}.md shape" $
      classifyPath "chapters/notes.md" `shouldBe` OtherFile

    it "does not recognize ch{N}.md outside a chapters/ directory" $
      classifyPath "ch1.md" `shouldBe` OtherFile

    it "falls back to OtherFile for anything else" $
      classifyPath "characters/alice.md" `shouldBe` OtherFile

  describe "buildLibraryTree" $ do
    it "groups chapter files under a synthesized chapters/ folder node" $ do
      let tree = buildLibraryTree ["chapters/ch1.md", "chapters/ch2.md"]
      map lnKind tree `shouldBe` [Folder]
      map lnPath tree `shouldBe` ["chapters"]
      map lnKind (lnChildren (head tree)) `shouldBe` [Chapter 1, Chapter 2]

    it "sorts chapters numerically, not alphabetically" $ do
      let tree = buildLibraryTree ["chapters/ch10.md", "chapters/ch2.md", "chapters/ch1.md"]
          chapters = lnChildren (head tree)
      map lnKind chapters `shouldBe` [Chapter 1, Chapter 2, Chapter 10]

    it "keeps a top-level file as its own root node" $ do
      let tree = buildLibraryTree ["outline.md"]
      map (\n -> (lnPath n, lnKind n)) tree `shouldBe` [("outline.md", StoryOutline)]

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
      lnKind (descend (head tree)) `shouldBe` Chapter 1

    it "every node's own path is populated, not just leaves" $ do
      let tree = buildLibraryTree ["a/b/ch.md"]
      lnPath (head tree) `shouldBe` "a"
      lnPath (head (lnChildren (head tree))) `shouldBe` "a/b"

  describe "chapterUnits" $ do
    it "pairs a chapter with its own beat sheet by number" $ do
      let tree = buildLibraryTree ["chapters/ch1.md", "chapters/ch1.outline.md"]
          [u] = chapterUnits tree
      cuNumber u `shouldBe` 1
      cuChapterPath u `shouldBe` Just "chapters/ch1.md"
      cuOutlinePath u `shouldBe` Just "chapters/ch1.outline.md"

    it "a beat sheet with no prose yet is still its own unit" $ do
      let tree = buildLibraryTree ["chapters/ch3.outline.md"]
          [u] = chapterUnits tree
      cuNumber u `shouldBe` 3
      cuChapterPath u `shouldBe` Nothing
      cuOutlinePath u `shouldBe` Just "chapters/ch3.outline.md"

    it "a chapter with no beat sheet is still its own unit" $ do
      let tree = buildLibraryTree ["chapters/ch1.md"]
          [u] = chapterUnits tree
      cuOutlinePath u `shouldBe` Nothing

    it "orders units by chapter number, not discovery order" $ do
      let tree = buildLibraryTree ["chapters/ch10.md", "chapters/ch2.outline.md", "chapters/ch1.md"]
      map cuNumber (chapterUnits tree) `shouldBe` [1, 2, 10]

    it "carries the chapter's own heading, populated separately from structure" $ do
      let tree = buildLibraryTree ["chapters/ch1.md"]
          setHeading n
            | lnKind n == Chapter 1 = n { lnHeading = Just "Ch1 title" }
            | otherwise             = n { lnChildren = map setHeading (lnChildren n) }
          headed = map setHeading tree
          [u] = chapterUnits headed
      cuHeading u `shouldBe` Just "Ch1 title"

    it "recognizes chapters/outlines regardless of surrounding folder nesting" $ do
      let tree = buildLibraryTree ["series/epic/chapters/ch1.md", "series/epic/chapters/ch1.outline.md"]
          [u] = chapterUnits tree
      cuChapterPath u `shouldBe` Just "series/epic/chapters/ch1.md"
      cuOutlinePath u `shouldBe` Just "series/epic/chapters/ch1.outline.md"
