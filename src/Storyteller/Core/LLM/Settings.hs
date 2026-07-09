{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | Per-role sparse config-override records: what a @$key.llmsettings.yaml@
--   (see 'Storyteller.Core.Prompt.getConfig') decodes into before being
--   turned into @['UniversalLLM.ModelConfig' role]@ via
--   'UniversalLLM.Settings.toModelConfigs'.
--
--   One record per role, not one generic record for all roles: a role's
--   'RoleSettings' can only mention fields its model actually has the
--   capability for, because 'UniversalLLM.Settings.ApplySetting' (which
--   'UniversalLLM.Settings.toModelConfigs' relies on via
--   'UniversalLLM.Settings.GApplySettings') requires that capability's
--   instance to exist on the role -- e.g. adding a @reasoning@ field to
--   'ProseSettings' would fail to compile, since 'Storyteller.Core.LLM.Role.
--   ProseModel' has no 'UniversalLLM.HasReasoning' instance. That's the
--   enforcement mechanism, not a convention to remember.
module Storyteller.Core.LLM.Settings
  ( RoleSettings
  , ProseSettings(..)
  , AgentSettings(..)
  ) where

import Data.Aeson (FromJSON(..), withObject, (.:?))
import GHC.Generics (Generic)

import UniversalLLM.Settings (TemperatureSetting(..), MaxTokensSetting(..), ReasoningSetting(..))

import Storyteller.Core.LLM.Role (ProseModel, AgentModel)

-- | Maps a role's model type to the settings record 'Storyteller.Core.Prompt.
--   getConfig' decodes its override file into.
type family RoleSettings model

-- | 'Storyteller.Core.LLM.Role.ProseModel' only ever needs sampling knobs --
--   it declares no 'UniversalLLM.HasReasoning' capability, so there is no
--   @reasoning@ field here (see this module's Haddock on why that's a
--   compile error, not an omission left to discipline).
data ProseSettings = ProseSettings
  { psTemperature :: Maybe TemperatureSetting
  , psMaxTokens   :: Maybe MaxTokensSetting
  } deriving (Generic)

instance FromJSON ProseSettings where
  parseJSON = withObject "ProseSettings" $ \v -> ProseSettings
    <$> (fmap TemperatureSetting <$> v .:? "temperature")
    <*> (fmap MaxTokensSetting   <$> v .:? "maxTokens")

-- | 'Storyteller.Core.LLM.Role.AgentModel' additionally declares
--   'UniversalLLM.HasReasoning', so @reasoning@ is a valid override here.
data AgentSettings = AgentSettings
  { asTemperature :: Maybe TemperatureSetting
  , asMaxTokens   :: Maybe MaxTokensSetting
  , asReasoning   :: Maybe ReasoningSetting
  } deriving (Generic)

instance FromJSON AgentSettings where
  parseJSON = withObject "AgentSettings" $ \v -> AgentSettings
    <$> (fmap TemperatureSetting <$> v .:? "temperature")
    <*> (fmap MaxTokensSetting   <$> v .:? "maxTokens")
    <*> (fmap ReasoningSetting   <$> v .:? "reasoning")

type instance RoleSettings ProseModel = ProseSettings
type instance RoleSettings AgentModel = AgentSettings
