{-# LANGUAGE OverloadedLists     #-}

module Qi.AWS.KF.Render (toResources) where

import           Control.Lens
import           Protolude        hiding (all)
import           Qi.AWS.Resource
import           Qi.AWS.Types
import           Qi.Config
import           Qi.AWS.KF
import           Stratosphere     (ResourceProperties (KinesisFirehoseDeliveryStreamProperties),
                                   Resources (Resources), Val (GetAtt, Literal))
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
toResources config = Resources $ toResource <$> kfs
  where
    kfs = all config

    toResource (kf :: Kf) = (
      S.resource (unLogicalId lid) $
        KinesisFirehoseDeliveryStreamProperties $ S.kinesisFirehoseDeliveryStream
          & S.kfdsDeliveryStreamName ?~ Literal (unPhysicalId pid)
          & S.kfdsDeliveryStreamType ?~ Literal "DirectPut"
          & S.kfdsS3DestinationConfiguration ?~ s3Dest
          {- & S.kfdsExtendedS3DestinationConfiguration ?~ lbdConfigs -}
          {- & S.kfdsKinesisStreamSourceConfiguration ?~ -}
      )
      {- & S.resourceDependsOn ?~ reqs -}

      where
        lid = logicalId config kf
        pid = physicalId config kf

        s3Dest = S.kinesisFirehoseDeliveryStreamS3DestinationConfiguration
                  bucketARNarg
                  bufferingHintsarg
                  compressionFormatarg
                  roleARNarg
                  {- & kfdssdcCloudWatchLoggingOptions ?~ -}
                  {- & kfdssdcEncryptionConfiguration ?~ -}
                  {- & kfdssdcPrefix ?~ -}

        bucketARNarg = undefined
        bufferingHintsarg = undefined
        compressionFormatarg = undefined
        roleARNarg = undefined
