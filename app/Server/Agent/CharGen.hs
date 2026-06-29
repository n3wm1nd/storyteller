{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Server.Agent.CharGen
  ( CharGenReq(..)
  , CharGenResp(..)
  , handleCharGen
  ) where

import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Polysemy
import Polysemy.Error (throw)
import Servant (Handler)

import qualified Data.Yaml as Yaml

import Server.Env (ServerEnv(..))
import Server.Run (runRequest)
import Storyteller.Agent.CharGen ( charGenCommit, ScenarioTemplate(..)
                                 , RngSeed(..), unSheet )
import Storyteller.Git (BranchTag(..), runBranchAndFS)
import Storyteller.Storage (StoryStorage, getBranch, createBranch)
import Storyteller.Types (BranchName(..), TickId(..))

data CharBranch

data CharGenReq = CharGenReq
  { charGenBranch   :: T.Text
  , charGenFile     :: FilePath
  , charGenScenario :: T.Text
  , charGenSeed     :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON CharGenReq
instance ToJSON CharGenReq

data CharGenResp = CharGenResp
  { charGenRespSheet :: T.Text
  , charGenRespTick  :: T.Text
  , charGenRespSeed  :: Int
  } deriving (Show, Generic)

instance ToJSON CharGenResp
instance FromJSON CharGenResp

handleCharGen :: ServerEnv -> CharGenReq -> Handler CharGenResp
handleCharGen env req = runRequest env $ do
  let branch = BranchName (charGenBranch req)
  template <- case Yaml.decodeEither' (TE.encodeUtf8 (charGenScenario req)) of
    Left  err -> throw (Yaml.prettyPrintParseException err)
    Right val -> return (ScenarioTemplate val)
  getBranch branch >>= \case
    Nothing -> void $ createBranch branch
    Just _  -> return ()
  runBranchAndFS @CharBranch branch $ do
    (sheet, RngSeed seed, tid) <-
      charGenCommit @CharBranch template (RngSeed <$> charGenSeed req) (charGenFile req)
    return CharGenResp
      { charGenRespSheet = unSheet sheet
      , charGenRespTick  = unTickId tid
      , charGenRespSeed  = seed
      }
