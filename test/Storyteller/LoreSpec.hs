{-# LANGUAGE OverloadedStrings #-}

-- | 'Storyteller.Writer.Lore.parseAliases' pins: a plain bold-label markdown
--   line, recognized only within the file's first section (the @# Title@
--   down to the next heading), case-insensitive label match, comma-split
--   values. See the alias-triggered lore-mentions design.
module Storyteller.LoreSpec (spec) where

import Test.Hspec

import Storyteller.Writer.Lore (parseAliases)

spec :: Spec
spec = do
  describe "parseAliases" $ do
    it "parses a bold-label aliases line in the first section" $
      parseAliases "# King Aldric\n\n**Aliases:** the King, His Majesty, the old king\n\nHe ruled for forty years.\n"
        `shouldBe` ["the King", "His Majesty", "the old king"]

    it "matches the label case-insensitively and without bold markers" $
      parseAliases "# King Aldric\n\naliases: the King, His Majesty\n"
        `shouldBe` ["the King", "His Majesty"]

    it "ignores an aliases-looking line outside the first section" $
      parseAliases "# King Aldric\n\nHe ruled for forty years.\n\n## History\n\n**Aliases:** the King\n"
        `shouldBe` []

    it "returns nothing when the file declares no aliases" $
      parseAliases "# King Aldric\n\nHe ruled for forty years.\n"
        `shouldBe` []

    it "drops empty entries and surrounding whitespace" $
      parseAliases "# King Aldric\n\n**Aliases:**  the King ,, His Majesty  \n"
        `shouldBe` ["the King", "His Majesty"]

    it "returns nothing for a file with no heading at all" $
      parseAliases "just some freeform notes\nno title here\n"
        `shouldBe` []
