{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared test interpreter stack for Server.Branch and Server.File specs.
module Server.TestStack
  ( TestEffects
  , testStack
  ) where

import Polysemy
import Polysemy.Error (Error, runError)
import Polysemy.Fail (Fail, failToError)
import Polysemy.State (evalState, State)

import Git.Mock
import Runix.Git (Git)
import Runix.Logging (Logging, loggingNull)

import Storyteller.Git
import Storyteller.Storage (StoryStorage)

type TestEffects r = StoryStorage : Git : State GitState : Logging : Fail : Error String : r

testStack
  :: Sem (TestEffects r) a
  -> Sem r (Either String a)
testStack =
    runError @String
  . failToError id
  . loggingNull
  . evalState emptyGitState
  . runGitMock
  . runStoryStorageGit
