{-# LANGUAGE TemplateHaskell #-}

module Qi.AWS.Lambda.EventSourceMapping where

import Control.Lens
import Data.Default (Default, def)
import Data.Text (Text)
import Protolude
import Qi.AWS.ARN
import Qi.AWS.Renderable
import Qi.AWS.Types
import qualified Stratosphere as S
import qualified Stratosphere.Lambda.EventSourceMapping as S 

data LambdaEventSourceMappingProfile = LambdaEventSourceMappingProfile
  { _lesmpBatchSize :: Integer
  }
  deriving (Eq, Show)

makeLenses ''LambdaEventSourceMappingProfile

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

instance AwsResource LambdaEventSourceMapping where
  type ResourceType LambdaEventSourceMapping = 'LambdaEventSourceMappingResource

instance Renderable LambdaEventSourceMapping where
  render appName (lid, rule@LambdaEventSourceMapping {source, functionId, profile}) =
    S.resource (show lid) $
        S.mkEventSourceMapping lambdaArn
          & S.set @"EventSourceArn" sourceArn
          & S.set @"BatchSize" (S.Literal $ profile ^. lesmpBatchSize)
          & S.set @"Enabled" (S.Literal True)
    where
      sourceArn = S.Literal (show source :: Text)
      lambdaArn = S.GetAtt (show functionId) "Arn"
