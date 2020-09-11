{-# LANGUAGE TemplateHaskell #-}

module Qi.AWS.Lambda.EventSourceMapping where

import Control.Lens
import Data.Default (Default, def)
import Data.Text (Text)
import Protolude
import Qi.AWS.ARN
import Qi.AWS.Renderable
import Qi.AWS.Types
import Stratosphere (Val (..))
import qualified Stratosphere as S

data LambdaEventSourceMappingProfile = LambdaEventSourceMappingProfile
  { _lesmpBatchSize :: Integer
  }
  deriving (Eq, Show)

instance Default LambdaEventSourceMappingProfile where
  def =
    LambdaEventSourceMappingProfile
      { _lesmpBatchSize = 10
      }

data LambdaEventSourceMapping = LambdaEventSourceMapping
  { source :: Arn,
    functionId :: LambdaId,
    profile :: LambdaEventSourceMappingProfile
  }
  deriving (Eq, Show)

makeLenses ''LambdaEventSourceMappingProfile

instance AwsResource LambdaEventSourceMapping where
  type ResourceType LambdaEventSourceMapping = 'LambdaEventSourceMappingResource

instance Renderable LambdaEventSourceMapping where
  render appName (lid, rule@LambdaEventSourceMapping {source, functionId, profile}) =
    S.resource (show lid) $
      S.lambdaEventSourceMapping lambdaArn sourceArn
        & S.lesmBatchSize ?~ Literal (profile ^. lesmpBatchSize)
        & S.lesmEnabled ?~ Literal True
    where
      sourceArn = Literal (show source)
      lambdaArn = GetAtt (show functionId) "Arn"
