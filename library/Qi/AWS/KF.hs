{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Qi.AWS.KF where

import           Control.Lens
import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Default        (Default, def)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as SHM
import           GHC.Show            (Show (..))
import           Protolude as P
import qualified Stratosphere    as S
import  Stratosphere (Val(..))

import Qi.AWS.Service
import           Qi.AWS.Types
import           Qi.AWS.IAM
import Qi.AWS.S3
import           Qi.AWS.ARN
import           Qi.AWS.Renderable


data KfStreamProfile = KfStreamProfile
  { _kfpExistence    :: ResourceExistence
  }
  deriving (Eq, Show)
makeLenses ''KfStreamProfile
instance Default KfStreamProfile where
  def = KfStreamProfile
    { _kfpExistence = ShouldCreate
    }

-- TODO: parameterize the dest
data KfStream = KfStream {
    _kfProfile :: KfStreamProfile
  , _kfRole    :: RoleId
  , _kfBucket  :: S3BucketId
  }
  deriving (Eq, Show)
makeLenses ''KfStream
instance AwsResource KfStream where
  type ResourceType KfStream = 'KfStreamResource
instance Renderable KfStream where
  render appName (lid, KfStream{ _kfRole, _kfBucket }) = (
      S.resource (P.show lid) $
        -- https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-kinesisfirehose-deliverystream.html
        S.kinesisFirehoseDeliveryStream
          & S.kfdsDeliveryStreamName ?~ Literal (P.show pid)
          & S.kfdsDeliveryStreamType ?~ Literal "DirectPut"
          & S.kfdsS3DestinationConfiguration ?~ s3Dest
          {- & S.kfdsExtendedS3DestinationConfiguration ?~ lbdConfigs -}
          {- & S.kfdsKinesisStreamSourceConfiguration ?~ -}
      )
      {- & S.resourceDependsOn ?~ reqs -}

      where
        pid = toPhysicalId appName lid

        -- https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-kinesisfirehose-deliverystream-s3destinationconfiguration.html
        s3Dest = S.kinesisFirehoseDeliveryStreamS3DestinationConfiguration
                  bucketArn
                  bufferingHints
                  compressionFormat
                  roleArn
                  {- & kfdssdcCloudWatchLoggingOptions ?~ -}
                  {- & kfdssdcEncryptionConfiguration ?~ -}
                   & S.kfdssdcPrefix ?~ prefix

        bucketArn = GetAtt (P.show _kfBucket) "Arn"
        -- TODO: put this in profile
        bufferingHints = S.KinesisFirehoseDeliveryStreamBufferingHints (Literal 60) (Literal 1) -- & kfdsbhIntervalInSeconds ?~ Literal 1
                                                                      -- & kfdsbhSizeInMBs .~ Literal 3
        compressionFormat = Literal S.KFS3Uncompressed

        -- The ARN of an AWS Identity and Access Management (IAM) role that grants Kinesis Data Firehose access to your Amazon S3 bucket and AWS KMS (if you enable data encryption).
        roleArn = GetAtt (P.show _kfRole) "Arn"

        -- A prefix that Kinesis Data Firehose adds to the files that it delivers to the Amazon S3 bucket. The prefix helps you identify the files that Kinesis Data Firehose delivered.
        prefix = "mydata"

data KfConfig = KfConfig {
    _idToStream :: HashMap KfStreamId KfStream
  }
  deriving (Eq, Show)
makeLenses ''KfConfig
instance Default KfConfig where
  def = KfConfig {
      _idToStream     = SHM.empty
    }
