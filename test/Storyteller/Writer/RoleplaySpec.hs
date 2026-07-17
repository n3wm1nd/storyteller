{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Storyteller.Writer.RoleplaySpec (spec) where

import qualified Data.Text as T
import Test.Hspec

import UniversalLLM (Message(..))

import Storyteller.Core.LLM.Role (AgentModel)
import Storyteller.Writer.Agent (ContextBlock(..), CharContextBlock(..), CharSummary(..))
import Storyteller.Writer.Agent.Roleplay (characterOpeningMessages, reflectOpeningMessages)

opening :: CharSummary -> [ContextBlock] -> [FilePath] -> T.Text -> [Message AgentModel]
opening = characterOpeningMessages "Ren"

reflect :: CharSummary -> T.Text -> [FilePath] -> T.Text -> [Message AgentModel]
reflect = reflectOpeningMessages "Ren"

noSummary :: CharSummary
noSummary = CharSummary [] [] []

allText :: [Message AgentModel] -> [T.Text]
allText msgs = [ t | m <- msgs, t <- case m of UserText t' -> [t']; AssistantText t' -> [t']; _ -> [] ]

-- | How many messages contain this exact substring -- the thing that must
--   stay at 1 for growing journal content, or a caller is paying to resend
--   (and a provider is being asked to reconcile) the same material twice.
occurrences :: T.Text -> [Message AgentModel] -> Int
occurrences needle msgs = length (filter (needle `T.isInfixOf`) (allText msgs))

-- | Worst case for a provider that concatenates adjacent same-role
--   messages -- merge every run of consecutive same-role messages into one.
--   Used to prove the *reason* every labelled section is a role-switching
--   pair: even under this worst case, stable content and the always-
--   changing journal must never end up in the same merged message.
mergeSameRole :: [Message m] -> [Message m]
mergeSameRole (UserText a : UserText b : rest)           = mergeSameRole (UserText (a <> "\n\n" <> b) : rest)
mergeSameRole (AssistantText a : AssistantText b : rest) = mergeSameRole (AssistantText (a <> "\n\n" <> b) : rest)
mergeSameRole (m : rest)                                  = m : mergeSameRole rest
mergeSameRole []                                          = []

spec :: Spec
spec = do
  describe "characterOpeningMessages" $ do

    it "with no own-branch context, still opens with the identity note and closes with the question" $ do
      let msgs = opening noSummary [] [] "what do you do?"
      case msgs of
        (UserText identity : _) -> identity `shouldSatisfy` ("Ren" `T.isInfixOf`)
        other                    -> expectationFailure ("expected identity note first, got " <> show other)
      case reverse msgs of
        (UserText tail_ : _) -> tail_ `shouldSatisfy` ("what do you do?" `T.isInfixOf`)
        other                 -> expectationFailure ("expected the question last, got " <> show (reverse other))

    it "puts the journal content exactly once, never fused into the stable context" $ do
      let cs = CharSummary
            { csSheet   = [CharContextBlock "### sheet.md\n\n# Ren"]
            , csContext = [CharContextBlock "### tasks.md\n\nkeep Elias distracted"]
            , csJournal = [CharContextBlock "### journal.md\n\nI keep thinking about the mark."]
            }
          msgs = opening cs [] [] "what do you ask Iskra?"
      occurrences "I keep thinking about the mark." msgs `shouldBe` 1
      occurrences "keep Elias distracted" msgs `shouldBe` 1
      occurrences "# Ren" msgs `shouldBe` 1

    it "places the journal message directly before the final scene/question message -- nothing sits between them" $ do
      let cs = CharSummary
            { csSheet   = [CharContextBlock "### sheet.md\n\n# Ren"]
            , csContext = [CharContextBlock "### tasks.md\n\nsome tasks"]
            , csJournal = [CharContextBlock "### journal.md\n\nsome journal entry"]
            }
          msgs = opening cs [] [] "what do you ask?"
      case reverse msgs of
        (finalMsg : AssistantText journalContent : UserText _label : _rest) -> do
          finalMsg `shouldSatisfy` (== UserText "You're being asked: what do you ask?")
          journalContent `shouldSatisfy` ("some journal entry" `T.isInfixOf`)
        other -> expectationFailure ("expected journal pair directly before the final message, got " <> show (reverse other))

    it "drops an empty journal section instead of emitting an empty pair" $ do
      let cs = CharSummary { csSheet = [CharContextBlock "### sheet.md\n\n# Ren"], csContext = [], csJournal = [] }
          msgs = opening cs [] [] "go"
      occurrences "## My own journal so far" msgs `shouldBe` 0

    it "survives a provider that concatenates adjacent same-role messages: the journal never ends up merged with stable content" $ do
      let cs = CharSummary
            { csSheet   = [CharContextBlock "### sheet.md\n\n# Ren"]
            , csContext = [CharContextBlock "### tasks.md\n\nsome tasks"]
            , csJournal = [CharContextBlock "### journal.md\n\nsome journal entry"]
            }
          msgs      = opening cs [] [] "what happens next?"
          flattened = mergeSameRole msgs
          journalMsg = [ t | m <- flattened, t <- allText [m], "some journal entry" `T.isInfixOf` t ]
      case journalMsg of
        [t] -> do
          t `shouldNotSatisfy` ("# Ren" `T.isInfixOf`)
          t `shouldNotSatisfy` ("some tasks" `T.isInfixOf`)
        other -> expectationFailure ("expected journal content in exactly one merged message, got " <> show other)

  describe "reflectOpeningMessages" $ do

    it "puts the journal content exactly once and the narrative last" $ do
      let cs = CharSummary
            { csSheet   = [CharContextBlock "### sheet.md\n\n# Ren"]
            , csContext = []
            , csJournal = [CharContextBlock "### journal.md\n\nolder entry"]
            }
          msgs = reflect cs "Elias asked a pointed question." [] "Write the entry."
      occurrences "older entry" msgs `shouldBe` 1
      case reverse msgs of
        (UserText t : _) -> t `shouldSatisfy` ("Elias asked a pointed question." `T.isInfixOf`)
        other             -> expectationFailure ("expected the narrative in the last message, got " <> show (reverse other))
