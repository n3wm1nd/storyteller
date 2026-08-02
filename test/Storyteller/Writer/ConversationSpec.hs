{-# LANGUAGE OverloadedStrings #-}

module Storyteller.Writer.ConversationSpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import Storage.Tick (FileTick(..))
import Storyteller.Writer.Conversation (Turn(..), turnsFromFileTicks)

-- | A minimal atom 'FileTick' -- only the fields 'turnsFromFileTicks'
--   itself actually reads ('ftKind', 'ftContent', 'ftHidden' via
--   'ftFields') are meaningful here. @ftTickId@ defaults to @"t"@, fine
--   as long as nothing in the same list needs to reference it by id --
--   see 'idTick' for the cases (note refs) that do.
atomTick :: T.Text -> FileTick
atomTick content = FileTick
  { ftTickId  = "t"
  , ftKind    = "atom"
  , ftRefs    = []
  , ftFields  = []
  , ftMessage = content
  , ftContent = Just content
  , ftParent  = Nothing
  }

promptTick :: T.Text -> FileTick
promptTick msg = (atomTick msg) { ftKind = "prompt", ftContent = Nothing }

idTick :: T.Text -> FileTick -> FileTick
idTick tid ft = ft { ftTickId = tid }

noteTick :: T.Text -> [T.Text] -> FileTick
noteTick msg refs = (atomTick msg) { ftKind = "note", ftContent = Nothing, ftRefs = refs }

spec :: Spec
spec = describe "turnsFromFileTicks" $ do
  it "strips an assistant turn's trailing paragraph-boundary newlines" $
    turnsFromFileTicks [atomTick "The door creaked open.\n\n"]
      `shouldBe` [AssistantTurn "The door creaked open."]

  it "strips a run of several trailing newlines, not just one" $
    turnsFromFileTicks [atomTick "She stepped inside.\n\n\n\n"]
      `shouldBe` [AssistantTurn "She stepped inside."]

  it "leaves an assistant turn with no trailing newline untouched" $
    turnsFromFileTicks [atomTick "The last line of the chapter."]
      `shouldBe` [AssistantTurn "The last line of the chapter."]

  it "preserves internal blank lines between paragraphs within one atom" $
    turnsFromFileTicks [atomTick "First paragraph.\n\nSecond paragraph.\n\n"]
      `shouldBe` [AssistantTurn "First paragraph.\n\nSecond paragraph."]

  it "never trims a user (prompt) turn -- only assistant turns carry split artifacts" $
    turnsFromFileTicks [promptTick "Write the next scene.\n\n"]
      `shouldBe` [UserTurn "Write the next scene.\n\n"]

  it "substitutes a placeholder for an assistant turn whose content is empty" $
    turnsFromFileTicks [atomTick ""]
      `shouldBe` [AssistantTurn "(No text was written for this turn.)"]

  it "substitutes a placeholder for an assistant turn that is all newlines" $
    turnsFromFileTicks [atomTick "\n\n\n"]
      `shouldBe` [AssistantTurn "(No text was written for this turn.)"]

  it "substitutes a placeholder for an assistant turn that is only whitespace" $
    turnsFromFileTicks [atomTick "   \n  \n"]
      `shouldBe` [AssistantTurn "(No text was written for this turn.)"]

  it "keeps a blank assistant turn as a real role boundary between two prompts" $
    turnsFromFileTicks [promptTick "Write the next scene.", atomTick "", promptTick "Try again."]
      `shouldBe`
        [ UserTurn "Write the next scene."
        , AssistantTurn "(No text was written for this turn.)"
        , UserTurn "Try again."
        ]

  it "surfaces a note with no refs as a plain NoteTurn" $
    turnsFromFileTicks [noteTick "watch the pacing here" []]
      `shouldBe` [NoteTurn "watch the pacing here"]

  it "keeps a note in tick order relative to the exchange it comments on" $
    turnsFromFileTicks
      [ promptTick "Write the opening."
      , atomTick "The door creaked open."
      , noteTick "too melodramatic" []
      , promptTick "Try again."
      ]
      `shouldBe`
        [ UserTurn "Write the opening."
        , AssistantTurn "The door creaked open."
        , NoteTurn "too melodramatic"
        , UserTurn "Try again."
        ]

  it "quotes a note's single ref by resolved content ahead of its own text" $
    turnsFromFileTicks
      [ idTick "a1" (atomTick "The door creaked open.")
      , noteTick "too melodramatic" ["a1"]
      ]
      `shouldBe`
        [ AssistantTurn "The door creaked open."
        , NoteTurn "> The door creaked open.\ntoo melodramatic"
        ]

  it "quotes every ref a note carries, in ref order" $
    turnsFromFileTicks
      [ idTick "p1" (promptTick "Write the opening.")
      , idTick "a1" (atomTick "The door creaked open.")
      , noteTick "these two don't match" ["p1", "a1"]
      ]
      `shouldBe`
        [ UserTurn "Write the opening."
        , AssistantTurn "The door creaked open."
        , NoteTurn "> Write the opening.\n> The door creaked open.\nthese two don't match"
        ]

  it "silently drops a ref that doesn't resolve to any tick in the file" $
    turnsFromFileTicks [noteTick "orphaned ref" ["missing"]]
      `shouldBe` [NoteTurn "orphaned ref"]
