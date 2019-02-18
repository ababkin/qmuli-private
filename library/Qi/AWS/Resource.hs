{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies              #-}

module Qi.AWS.Resource where

import           Control.Lens hiding (mapping)
import           Data.Char            (isAlphaNum)
import           Data.Default         (Default, def)
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as SHM
import           Data.Maybe           (fromMaybe)
import           GHC.Show             (Show (..))
import           Protolude            hiding (show)
import qualified Protolude            as P
import           Qi.AWS.Types
import           Qi.Config
import Qi.AWS.S3
import Qi.AWS.Lambda
import Qi.AWS.KF


class (Show (LogicalId (ResourceType r)), Hashable (LogicalId (ResourceType r))) => AwsResource r where

  type ResourceType r :: AwsResourceType

  mapping
    :: Config
    -> SHM.HashMap (LogicalId (ResourceType r)) r

  all
    :: Config
    -> [ (LogicalId (ResourceType r), r) ]
  all = SHM.toList . mapping

  getById
    :: Config
    -> LogicalId (ResourceType r)
    -> r
  getById config lid =
    fromMaybe
      (panic $ "Could not reference resource with logical id: " <> P.show lid)
      $ SHM.lookup lid $ mapping config


type LambdaId = LogicalId (ResourceType Lambda)
instance AwsResource Lambda where
  type ResourceType Lambda = 'LambdaResource
  mapping = view $ lbdConfig . lbdIdToLambda

type KfId = LogicalId (ResourceType Kf)
instance AwsResource Kf where
  type ResourceType Kf = 'KinesisFirehoseResource
  mapping = view $ kfConfig . kfIdToKf

type S3BucketId = LogicalId (ResourceType S3Bucket)
instance AwsResource S3Bucket where
  type ResourceType S3Bucket = 'S3BucketResource
  mapping = view $ s3Config . s3IdToBucket
