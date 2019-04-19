{-# LANGUAGE OverloadedLists     #-}

module Qi.AWS.KF.Render (toResources) where

import           Data.Aeson
import           Control.Lens
import           Protolude        hiding (all)
import           Qi.AWS.Resource
import           Qi.AWS.ARN
import           Qi.AWS.Types
import           Qi.Config
import           Qi.AWS.KF
import           Stratosphere
import qualified Stratosphere     as S (kfdsDeliveryStreamName,
                                        kfdsDeliveryStreamType,
                                        kfdsExtendedS3DestinationConfiguration,
                                        kfdsKinesisStreamSourceConfiguration,
                                        kfdsS3DestinationConfiguration,
                                        kinesisFirehoseDeliveryStream,
                                        kinesisFirehoseDeliveryStreamS3DestinationConfiguration,
                                        resource, resourceDependsOn)


toResources
  :: Config
  -> Resources
toResources config@Config{ _appName } = Resources $ toResource <$> kfs
  where
    kfs = all config

    toResource (lid, Kf{ _kfBucket }) = (
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
                  {- & kfdssdcPrefix ?~ -}

        bucketArn = Literal . show $ toArn _kfBucket _appName
        bufferingHints = KinesisFirehoseDeliveryStreamBufferingHints (Literal 1) (Literal 3) -- & kfdsbhIntervalInSeconds ?~ Literal 1
                                                                      -- & kfdsbhSizeInMBs .~ Literal 3
        compressionFormat = Literal KFS3Uncompressed
        roleArn = Literal "blah"
