{-# LANGUAGE OverloadedLists     #-}

module Qi.AWS.KF.Render (toResources) where

import           Data.Aeson
import           Control.Lens
import           Protolude        hiding (all)
import           Stratosphere
import qualified Stratosphere     as S (kfdsDeliveryStreamName,
                                        kfdsDeliveryStreamType,
                                        kfdsExtendedS3DestinationConfiguration,
                                        kfdsKinesisStreamSourceConfiguration,
                                        kfdsS3DestinationConfiguration,
                                        kinesisFirehoseDeliveryStream,
                                        kinesisFirehoseDeliveryStreamS3DestinationConfiguration,
                                        resource, resourceDependsOn)

import           Qi.AWS.Resource
import           Qi.AWS.ARN
import           Qi.AWS.Types
import           Qi.Config
import           Qi.AWS.KF


toResources
  :: Config
  -> Resources
toResources config@Config{ _appName } = Resources $ toResource <$> kfs
  where
    kfs = all config

    toResource (lid, KfStream{ _kfRole, _kfBucket }) = (
      S.resource (show lid) $
        -- https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-kinesisfirehose-deliverystream.html
        S.kinesisFirehoseDeliveryStream
          & S.kfdsDeliveryStreamName ?~ Literal (show pid)
          & S.kfdsDeliveryStreamType ?~ Literal "DirectPut"
          & S.kfdsS3DestinationConfiguration ?~ s3Dest
          {- & S.kfdsExtendedS3DestinationConfiguration ?~ lbdConfigs -}
          {- & S.kfdsKinesisStreamSourceConfiguration ?~ -}
      )
      {- & S.resourceDependsOn ?~ reqs -}

      where
        pid = toPhysicalId _appName lid

        -- https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-kinesisfirehose-deliverystream-s3destinationconfiguration.html
        s3Dest = S.kinesisFirehoseDeliveryStreamS3DestinationConfiguration
                  bucketArn
                  bufferingHints
                  compressionFormat
                  roleArn
                  {- & kfdssdcCloudWatchLoggingOptions ?~ -}
                  {- & kfdssdcEncryptionConfiguration ?~ -}
                   & kfdssdcPrefix ?~ prefix

        bucketArn = GetAtt (show _kfBucket) "Arn" -- Literal . show $ toArn _kfBucket _appName
        -- TODO: put this in profile
        bufferingHints = KinesisFirehoseDeliveryStreamBufferingHints (Literal 60) (Literal 1) -- & kfdsbhIntervalInSeconds ?~ Literal 1
                                                                      -- & kfdsbhSizeInMBs .~ Literal 3
        compressionFormat = Literal KFS3Uncompressed

        -- The ARN of an AWS Identity and Access Management (IAM) role that grants Kinesis Data Firehose access to your Amazon S3 bucket and AWS KMS (if you enable data encryption).
        roleArn = GetAtt (show _kfRole) "Arn"

        -- A prefix that Kinesis Data Firehose adds to the files that it delivers to the Amazon S3 bucket. The prefix helps you identify the files that Kinesis Data Firehose delivered.
        prefix = "mydata"
