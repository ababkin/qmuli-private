{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.AWS.S3.Render (toResources) where

import           Control.Lens
import           Protolude       hiding (all)
import           Qi.AWS.Resource
import           Qi.AWS.S3       (S3Bucket (..), S3EventConfig (..), event,
                                  lbdId, s3bEventConfigs, s3bProfile,
                                  s3bpExistence)
import           Qi.AWS.Types
import           Qi.Config
import           Stratosphere    (CannedACL (..),
                                  ResourceProperties (S3BucketProperties),
                                  Resources (Resources), Val (GetAtt, Literal))
import qualified Stratosphere    as S (resource, resourceDependsOn, s3Bucket,
                                       s3BucketLambdaConfiguration,
                                       s3BucketNotificationConfiguration,
                                       sbAccessControl, sbBucketName,
                                       sbNotificationConfiguration,
                                       sbncLambdaConfigurations)


toResources
  :: Config
  -> Resources
toResources config@Config{ _appName } = Resources $ toResource <$> buckets
  where
    buckets = filter (\(_, s3b) -> s3b ^. s3bProfile . s3bpExistence /= AlreadyExists) $ all config

    toResource (lid, bucket) = (
      S.resource (show lid) $
        S3BucketProperties $ S.s3Bucket
          & S.sbBucketName    ?~ Literal (show pid)
          & S.sbAccessControl ?~ Literal PublicReadWrite
          & S.sbNotificationConfiguration ?~ lbdConfigs
      )
      & S.resourceDependsOn ?~ reqs

      where
        pid = toPhysicalId _appName lid
        eventConfigs = bucket ^. s3bEventConfigs

        reqs = concat $
          (\eventConfig ->
            let
              eventLambdaLogicalId = eventConfig ^. lbdId
            in
            [ show (castLogicalIdResource eventLambdaLogicalId :: LogicalId 'LambdaPermissionResource)
            , show eventLambdaLogicalId
            ]
          ) <$> eventConfigs


        lbdConfigs = S.s3BucketNotificationConfiguration
          & S.sbncLambdaConfigurations ?~ map lbdConf eventConfigs

        lbdConf s3EventConfig =
          S.s3BucketLambdaConfiguration
            (Literal . show $ s3EventConfig ^. event)
            (GetAtt (show $ s3EventConfig ^. lbdId) "Arn")
