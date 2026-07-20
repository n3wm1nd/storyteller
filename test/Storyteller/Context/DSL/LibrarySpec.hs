{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The default library ("Storyteller.Context.DSL.Library") actually
--   composes against a real (mock-git-backed) branch -- a clean compile
--   here proves nothing about whether 'contextMain' correctly threads
--   'contextLore'\/'contextChapters'\/'contextStyle' into its own exports
--   (a wrong parameter order, a swapped @in@, would still typecheck).
module Storyteller.Context.DSL.LibrarySpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Polysemy (Members, Sem, run)
import Polysemy.Fail (Fail)

import Runix.Git (Git)

import qualified Storage.Core as Core
import qualified Storage.Ops as Ops
import Storyteller.Core.Git (runBranchOpGit, runStorage)
import Storyteller.Core.Storage (StoryStorage, createBranch, getBranch)
import Storyteller.Core.Types (Branch(..), BranchName(..), TickId(..))

import Server.Core.Branch (Main)
import Server.TestStack

import Storyteller.Context.DSL.Compile (bval)
import Storyteller.Context.DSL.Library
  (contextChapters, contextLore, contextMain, contextMentionFilter, contextStyle)
import Storyteller.Context.DSL.Value

seedBranch :: Text -> [(FilePath, Text)] -> Sem (StoryStorage : TestEffects '[]) ()
seedBranch name files = do
  _ <- createBranch (BranchName name)
  runBranchOpGit @Main (BranchName name)
    (mapM_ (\(path, content) -> runStorage @Main (Ops.addAtom path content)) files)

runDslOn :: BranchName -> Action a -> Sem (StoryStorage : TestEffects '[]) a
runDslOn bname act = resolveBranch bname >>= \case
  Nothing -> fail ("branch not found: " <> T.unpack (unBranchName bname))
  Just h  -> fst <$> Core.runStoreT h (runAction act)

entryTexts :: Value -> Action (Map Text Text)
entryTexts v = Map.fromList <$>
  mapM (\(k, act) -> (,) k . messagesText <$> (valueDefault =<< act)) (valueEntries v)

spec :: Spec
spec = do
  contextMainSpec
  contextMentionFilterSpec

contextMainSpec :: Spec
contextMainSpec = describe "contextMain (the default context.main library entry)" $
  it "exports lore/chapters/other/style as distinguished, unflattened buckets -- chapters in natural order with User/Assistant pairing, style separate, a stray note caught by other, chat scratch hidden" $
    run (testStack $ do
      seedBranch "main"
        [ ("lore/notes.md", "a hand-authored note")
        , ("style.md", "write in past tense")
        , ("chapters/ch11.md", "chapter eleven prose")
        , ("chapters/ch2.md", "chapter two prose")
        , ("chat/scratch.md", "chat scratch, never lore or a chapter")
        , ("todo.md", "a stray root note, filed under neither lore/ nor chapters/")
        ]
      runDslOn (BranchName "main") go)
    `shouldBe` Right
      ( ["lore", "chapters", "other", "style"]
      , Map.fromList [("lore/notes.md", "a hand-authored note")]
      , [ "## Chapter: chapters/ch2.md\nchapter two prose"
        , "## Chapter: chapters/ch11.md\nchapter eleven prose"
        ]
      , Map.fromList [("todo.md", "a stray root note, filed under neither lore/ nor chapters/")]
      , "write in past tense"
      )
  where
    bucket name v = case lookup name (valueEntries v) of
      Just act -> act
      Nothing  -> pure emptyValue
    go = do
      v <- contextMain (bval contextLore) (bval contextChapters) (bval contextStyle)
      loreTxt      <- entryTexts =<< bucket "lore" v
      chaptersVal  <- bucket "chapters" v
      chapterTexts <- mapM (\(_, act) -> messagesText <$> (valueDefault =<< act)) (valueEntries chaptersVal)
      otherTxt     <- entryTexts =<< bucket "other" v
      style        <- messagesText <$> (valueDefault =<< bucket "style" v)
      pure (map fst (valueEntries v), loreTxt, chapterTexts, otherTxt, style)

contextMentionFilterSpec :: Spec
contextMentionFilterSpec = describe "contextMentionFilter (the default context.mentionFilter library entry)" $
  it "is the identity: every candidate alias stays active by default" $
    run (testStack $ do
      seedBranch "main" [("sheet.md", "Aria is a wandering rogue.")]
      runDslOn (BranchName "main") go)
    `shouldBe` Right (Map.fromList [("Aria", "Aria is a wandering rogue.")])
  where
    aliases = pure Value
      { valueDefault = pure []
      , valueEntries = [("Aria", pure (leafValue [User "Aria is a wandering rogue."]))]
      }
    go = do
      v <- contextMentionFilter (bval aliases)
      entryTexts v
