{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.AWS.S3.Render (toResources) where

import           Control.Lens
import           Protolude               hiding (all)
import qualified Qi.AWS.Lambda.Accessors as L
import           Qi.AWS.Resource
import           Qi.AWS.S3               (S3Bucket (..), S3EventConfig (..),
                                          event, lbdId, s3bEventConfigs,
                                          s3bProfile, s3bpExistence)
import           Qi.AWS.Types
import           Qi.Config
import           Stratosphere            (CannedACL (..), ResourceProperties (S3BucketProperties),
                                          Resources (Resources),
                                          Val (GetAtt, Literal))
import qualified Stratosphere            as S (resource, resourceDependsOn,
                                               s3Bucket,
                                               s3BucketLambdaConfiguration,
                                               s3BucketNotificationConfiguration,
                                               sbAccessControl, sbBucketName,
                                               sbNotificationConfiguration,
                                               sbncLambdaConfigurations)


toResources
  :: Config
  -> Resources
toResources config = Resources $ toResource <$> buckets
  where
    buckets = filter (\s3b -> s3b ^. s3bProfile . s3bpExistence /= AlreadyExists) $ all config

    toResource bucket = (
      S.resource (unLogicalId bucketId) $
        S3BucketProperties $ S.s3Bucket
          & S.sbBucketName    ?~ Literal (unPhysicalId pBucketId)
          & S.sbAccessControl ?~ Literal PublicReadWrite
          & S.sbNotificationConfiguration ?~ lbdConfigs
      )
      & S.resourceDependsOn ?~ reqs

      where
        bucketId = logicalId config bucket
        pBucketId = physicalId config bucket
        eventConfigs = bucket ^. s3bEventConfigs

        reqs = concat $
          (\eventConfig ->
            let
              eventLambdaLogicalId = eventConfig ^. lbdId
              lbd = getById config eventLambdaLogicalId
            in
            [ L.getPermissionLogicalId lbd
            , unLogicalId eventLambdaLogicalId
            ]
          ) <$> eventConfigs


        lbdConfigs = S.s3BucketNotificationConfiguration
          & S.sbncLambdaConfigurations ?~ map lbdConf eventConfigs

        lbdConf s3EventConfig =
          S.s3BucketLambdaConfiguration
            (Literal . show $ s3EventConfig ^. event)
            (GetAtt (unLogicalId $ s3EventConfig ^. lbdId) "Arn")
