{-# LANGUAGE TemplateHaskell   #-}

module Qi.AWS.CW where

import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Default               (Default, def)
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as SHM
import           Protolude
import           Polysemy
import qualified Stratosphere    as S
import  Stratosphere (Val(..))

import           Qi.AWS.Types
import Qi.AWS.Renderable


data CwEvent = CwEvent -- {}
  deriving (Eq, Show)
instance FromJSON CwEvent where
  parseJSON v = pure CwEvent

data CwEventsRuleProfile =
    ScheduledEventProfile {
    _csepSchedule :: Text
  }
  | PatternedEventProfile {
    _cpepPattern :: Text
  }
  deriving (Eq, Show)
makeLenses ''CwEventsRuleProfile

data CwEventsRule = CwEventsRule {
    _cerName    :: Text
  , _cerProfile :: CwEventsRuleProfile
  , _cerLbdId   :: LambdaId
  }
  deriving (Eq, Show)
makeLenses ''CwEventsRule
instance AwsResource CwEventsRule where
  type ResourceType CwEventsRule = 'CwEventsRuleResource
instance Renderable CwEventsRule where
  render appName (lid, rule@CwEventsRule{ _cerLbdId }) =
      S.resource (show lid) $
        S.eventsRule
        & S.erName ?~ Literal (show pid)
        & S.erScheduleExpression ?~ Literal (rule ^. cerProfile . csepSchedule)
        & S.erState ?~ Literal S.ENABLED
        & S.erTargets ?~ [ target ]

      where
        target = S.eventsRuleTarget
                        tarn
                        (Literal tid)
        -- composite ID for the specific rule target (concat of the rule's and the lambda's pids)
        -- TODO: rethink this
        tid  = show $ hash (show pid <> "__" <> show lbdPid :: Text)
        tarn = GetAtt (show _cerLbdId) "Arn"
        pid  = toPhysicalId appName lid
        lbdPid  = toPhysicalId appName _cerLbdId

data CwConfig = CwConfig {
    _idToRule :: HashMap CwEventsRuleId CwEventsRule
  }
  deriving (Eq, Show)
instance Default CwConfig where
  def = CwConfig {
    _idToRule = SHM.empty
  }
makeLenses ''CwConfig
