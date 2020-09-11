{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Qi.AWS.S3
  ( S3Config (..),
    S3Bucket (..),
    S3BucketId,
    S3BucketProfile (..),
    S3Key (..),
    S3Object (..),
    S3Event (..),
    S3EventType (..),
    S3EventConfig (..),
    idToBucket,
    event,
    lbdId,
    s3bEventConfigs,
    s3bProfile,
    s3bpExistence,
    s3eObject,
    s3oBucketId,
    s3oKey,
  )
where

import Control.Lens
import Control.Monad.Fail (fail)
import Data.Aeson
import Data.Default (Default, def)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as SHM
import GHC.Show (Show (..))
import Protolude as P
import Qi.AWS.ARN
import Qi.AWS.Renderable
import Qi.AWS.Service
import Qi.AWS.Types
import Stratosphere (Val (..))
import qualified Stratosphere as S

data S3BucketProfile = S3BucketProfile
  { _s3bpExistence :: ResourceExistence
  }
  deriving (Eq, Show)

instance Default S3BucketProfile where
  def =
    S3BucketProfile
      { _s3bpExistence = ShouldCreate
      }

makeLenses ''S3BucketProfile

data S3EventType
  = S3ObjectCreatedAll
  | S3ObjectRemovedAll
  deriving (Eq)

instance Show S3EventType where
  show S3ObjectCreatedAll = "s3:ObjectCreated:*"
  show S3ObjectRemovedAll = "s3:ObjectRemoved:*"

newtype S3Key = S3Key Text
  deriving (Eq, Generic, ToJSON, FromJSON)

instance Show S3Key where
  show (S3Key key) = toS key

data S3Object = S3Object
  { _s3oBucketId :: S3BucketId,
    _s3oKey :: S3Key
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

makeLenses ''S3Object

instance ToArn S3Object where
  toArn s3obj appName =
    Arn
      { service = S3,
        region = "",
        resource = arnRes
      }
    where
      bucketId = s3obj ^. s3oBucketId
      objKey = s3obj ^. s3oKey
      arnRes = showPhysicalId (toPhysicalId appName bucketId) <> "/" <> P.show objKey

data S3Event = S3Event
  { _s3eObject :: S3Object
  }
  deriving (Eq, Show)

instance FromJSON S3Event where
  parseJSON = withObject "S3Event" $ \o -> do
    firstRecord <- headMay <$> o .: "Records"
    -- TODO: should we consider cases where there are more than one records? (probably yes)
    case firstRecord of
      Nothing ->
        fail "no records"
      Just record -> do
        s3 <- record .: "s3"
        bucketId <- (.: "name") =<< s3 .: "bucket"
        key <- (.: "key") =<< s3 .: "object"
        case parsePhysicalId bucketId of
          Left err ->
            fail $
              "could not parse s3 bucket physical id: " <> P.show bucketId
                <> ", error was: "
                <> P.show err
          Right pid ->
            pure . S3Event $ S3Object (toLogicalId pid) (S3Key key)

makeLenses ''S3Event

data S3EventConfig = S3EventConfig
  { _event :: S3EventType,
    _lbdId :: LambdaId
  }
  deriving (Eq, Show)

makeLenses ''S3EventConfig

-- https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-bucket.html

-- | Representation of a S3 Bucket resource
data S3Bucket = S3Bucket
  { _s3bProfile :: S3BucketProfile,
    _s3bEventConfigs :: [S3EventConfig]
  }
  deriving (Eq, Show)

instance Default S3Bucket where
  def =
    S3Bucket
      { _s3bProfile = def,
        _s3bEventConfigs = def
      }

makeLenses ''S3Bucket

instance AwsResource S3Bucket where
  type ResourceType S3Bucket = 'S3BucketResource

instance Renderable S3Bucket where
  render appName (lid, bucket) =
    ( S.resource (P.show lid) $
        S.s3Bucket
          & S.sbBucketName ?~ Literal (showPhysicalId pid)
          & S.sbAccessControl ?~ Literal S.PublicReadWrite
          & S.sbNotificationConfiguration ?~ lbdConfigs
    )
      & S.resourceDependsOn ?~ reqs
    where
      pid = toPhysicalId appName lid
      eventConfigs = bucket ^. s3bEventConfigs

      reqs =
        concat $
          ( \eventConfig ->
              let eventLambdaLogicalId = eventConfig ^. lbdId
               in [ P.show (castLogicalIdResource eventLambdaLogicalId :: LogicalId 'LambdaPermissionResource),
                    P.show eventLambdaLogicalId
                  ]
          )
            <$> eventConfigs

      lbdConfigs =
        S.s3BucketNotificationConfiguration
          & S.sbncLambdaConfigurations ?~ map lbdConf eventConfigs

      lbdConf s3EventConfig =
        S.s3BucketLambdaConfiguration
          (Literal . P.show $ s3EventConfig ^. event)
          (GetAtt (P.show $ s3EventConfig ^. lbdId) "Arn")

-- | This represents config for the S3 resources
data S3Config = S3Config
  { _idToBucket :: HashMap S3BucketId S3Bucket
  }
  deriving (Eq, Show)

instance Default S3Config where
  def =
    S3Config
      { _idToBucket = SHM.empty
      }

makeLenses ''S3Config
