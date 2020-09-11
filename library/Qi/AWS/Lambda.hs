{-# LANGUAGE TemplateHaskell #-}

module Qi.AWS.Lambda where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Default (Default, def)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as SHM
import Data.Text (Text)
import GHC.Show (Show (..))
import Polysemy
import Protolude as P
import Qi.AWS.ARN
import Qi.AWS.IAM
import Qi.AWS.Lambda.EventSourceMapping
import Qi.AWS.Lambda.Function
import Qi.AWS.Lambda.Permission
import Qi.AWS.Types

data LambdaConfig = LambdaConfig
  { _idToFunction :: HashMap LambdaId LambdaFunction,
    _idToEventSourceMapping :: HashMap LambdaMappingId LambdaEventSourceMapping,
    _idToPermission :: HashMap LambdaPermissionId LambdaPermission
  }
  deriving (Eq, Show)

instance Default LambdaConfig where
  def =
    LambdaConfig
      { _idToFunction = SHM.empty,
        _idToEventSourceMapping = SHM.empty,
        _idToPermission = SHM.empty
      }

makeLenses ''LambdaConfig
