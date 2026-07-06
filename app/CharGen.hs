{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | story-chargen: generate a character sheet from a scenario template.
--
-- ENV:
--   STORY_REPO     path to the git repository
--   STORY_BRANCH   entity branch to initialise (created if it does not exist)
--
-- ARGS:
--   <scenario.yaml>   scenario template to resolve
--   <sheet-file>      filename to write inside the branch (e.g. sheet.md)
--   [--seed N]        optional RNG seed (random if omitted)
--
-- STDOUT: the rendered character sheet.
module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)
import           System.Random (randomIO)

import qualified Data.Yaml as Yaml

import           Polysemy
import           Polysemy.Error (runError)
import           Polysemy.Fail (Fail)
import           Runix.FileSystem (FileSystem, FileSystemRead, FileSystemWrite, writeFile)
import           Runix.Logging (Logging)

import           Storyteller.Writer.Agent.CharGen
  (charGenAgent, ScenarioTemplate(..), RngSeed(..), unSheet)
import           Storyteller.Core.CLI.Env (StoryEnv(..), loadEnv)
import           Storyteller.Core.Git (BranchTag(..), BranchOp, runStorageEdit)
import qualified Storyteller.Core.StorageMonad as SM
import           Storyteller.Core.Runtime (runInfrastructure, runBranchAndFS, runStoryStorageGit)
import           Storyteller.Core.Storage (StoryStorage)
import           Storyteller.Core.Types (BranchName(..), TickId)

import           Prelude hiding (writeFile)

data CharBranch

main :: IO ()
main = do
  env  <- loadEnv
  args <- getArgs

  (scenarioPath, sheetFile, maybeSeed) <- parseArgs args

  raw <- Yaml.decodeFileEither scenarioPath >>= \case
    Left  err -> die ("YAML parse error: " <> Yaml.prettyPrintParseException err)
    Right val -> return val

  seed <- maybe randomIO return maybeSeed

  let branch = BranchName (envBranch env)
      sheet  = charGenAgent (ScenarioTemplate raw) (RngSeed seed)
      text   = unSheet sheet

  result <- runM . runError
    . runInfrastructure (envRepo env) (envEndpoint env)
    . runStoryStorageGit
    . runBranchAndFS @CharBranch branch
    $ charGenAction @CharBranch sheetFile text

  case result of
    Left  err -> hPutStrLn stderr ("Error: " <> err) >> exitFailure
    Right _   -> do
      putStrLn ("seed: " <> show seed)
      TIO.putStr text

charGenAction
  :: forall branch r
  .  Members '[ FileSystem      (BranchTag branch)
              , FileSystemRead  (BranchTag branch)
              , FileSystemWrite (BranchTag branch)
              , BranchOp branch
              , StoryStorage
              , Logging, Fail
              ] r
  => FilePath -> T.Text -> Sem r [(TickId, TickId)]
charGenAction sheetFile text = do
  writeFile @(BranchTag branch) sheetFile (TE.encodeUtf8 text)
  snd <$> runStorageEdit @branch (((),) <$> SM.commitFiles [sheetFile])

parseArgs :: [String] -> IO (FilePath, FilePath, Maybe Int)
parseArgs args = go args Nothing Nothing Nothing
  where
    go ("--seed" : n : rest) sc sf _ =
      case reads n of
        [(i, "")] -> go rest sc sf (Just i)
        _         -> die ("Invalid seed: " <> n)
    go (a : rest) Nothing  sf s = go rest (Just a) sf s
    go (a : rest) sc Nothing  s = go rest sc (Just a) s
    go []         (Just sc) (Just sf) s = return (sc, sf, s)
    go _          _         _         _ = die usage

    usage = "Usage: story-chargen <scenario.yaml> <sheet-file> [--seed N]"

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure
