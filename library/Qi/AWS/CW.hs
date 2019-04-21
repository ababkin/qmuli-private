{-# LANGUAGE TemplateHaskell   #-}

module Qi.AWS.CW where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Default               (Default, def)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as SHM
import           Protolude
import           Polysemy

import           Qi.AWS.Types


type LambdaId       = LogicalId 'LambdaResource
type CwEventsRuleId = LogicalId 'CwEventsRuleResource
type CwLambdaProgram effs = CwEvent -> Sem effs LBS.ByteString

data CwEventsRule = CwEventsRule {
    _cerName    :: Text
  , _cerProfile :: CwEventsRuleProfile
  , _cerLbdId   :: LambdaId
  }
  deriving (Eq, Show)

data CwConfig = CwConfig {
    _ccRules :: HashMap CwEventsRuleId CwEventsRule
  }
  deriving (Eq, Show)

instance Default CwConfig where
  def = CwConfig {
    _ccRules = SHM.empty
  }


data CwEventsRuleProfile =
    ScheduledEventProfile {
    _csepSchedule :: Text
  }
  | PatternedEventProfile {
    _cpepPattern :: Text
  }
  deriving (Eq, Show)

data CwEvent = CwEvent -- {}
  deriving (Eq, Show)

instance FromJSON CwEvent where
  parseJSON v = pure CwEvent

makeLenses ''CwEventsRuleProfile
makeLenses ''CwEventsRule
makeLenses ''CwConfig
