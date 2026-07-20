{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | 'Server.Writer.Lore.loreTree' -- in particular, that every declared
--   alias is run through @context.mentionFilter@ before it reaches a
--   'Storyteller.Writer.Lore.LoreNode': identity by default (every alias
--   stays), narrowed when a project overrides the definition (@aliases |
--   without(...)@), the same override mechanism every other @context.*@
--   definition already gets.
module Server.Writer.LoreSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Test.Hspec

import Polysemy
import Polysemy.Error (runError)
import Polysemy.Fail (failToError)
import Polysemy.State (evalState)

import Git.Mock (emptyGitState, runGitMock)
import Runix.Logging (loggingNull)

import Storyteller.Core.Context (interpretContextStorageMap)
import Storyteller.Core.Git (runBranchAndFS, runStorage, runStoryStorageGit)
import Storyteller.Core.LLM.Role (AgentModel, ProseModel)
import Storyteller.Core.Prompt (interpretPromptStorageMap)
import Storyteller.Core.Storage (createBranch)
import qualified Storage.Ops as Ops

import Server.Core.Branch (Main)
import Server.TestStack (stubLLM)
import Server.Writer.Lore (loreTree)
import Storyteller.Core.Types (BranchName(..))
import Storyteller.Writer.Lore (LoreNode(..))

spec :: Spec
spec = do
  identitySpec
  overrideSpec

identitySpec :: Spec
identitySpec = describe "loreTree" $
  it "keeps every declared alias active by default (context.mentionFilter is the identity)" $
    aliasesOf Map.empty [("notes.md", "A hand-authored note.\n\n**Aliases:** Foo, Bar\n")]
      `shouldBe` Right ["Foo", "Bar"]

overrideSpec :: Spec
overrideSpec = describe "loreTree with an overridden context.mentionFilter" $
  it "drops an alias the override excludes, keeping the rest" $
    aliasesOf
      (Map.fromList
        [ ("context.mentionFilter", T.unlines
            [ "aliases:"
            , "  in (aliases | without(\"Foo\")):"
            , "    for f in *:"
            , "      as f: read f"
            ])
        ])
      [("notes.md", "A hand-authored note.\n\n**Aliases:** Foo, Bar\n")]
      `shouldBe` Right ["Bar"]

aliasesOf :: Map.Map T.Text T.Text -> [(FilePath, T.Text)] -> Either String [T.Text]
aliasesOf overrides files = fmap (concatMap allAliases) (runLoreTestFull overrides files)

allAliases :: LoreNode -> [T.Text]
allAliases n = lnAliases n ++ concatMap allAliases (lnChildren n)

runLoreTestFull :: Map.Map T.Text T.Text -> [(FilePath, T.Text)] -> Either String [LoreNode]
runLoreTestFull overrides files =
  run
  . runError @String
  . failToError id
  . loggingNull
  . evalState emptyGitState
  . runGitMock
  . interpretContextStorageMap overrides
  . interpretPromptStorageMap Map.empty
  . stubLLM @AgentModel
  . stubLLM @ProseModel
  . runStoryStorageGit
  $ do
      _ <- createBranch (BranchName "story")
      runBranchAndFS @Main (BranchName "story") $ do
        mapM_ (\(path, content) -> runStorage @Main (Ops.addAtom path content)) files
        loreTree
