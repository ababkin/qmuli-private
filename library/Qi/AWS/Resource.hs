{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ScopedTypeVariables          #-}

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

  typeName
    :: r
    -> Text

  name
    :: r
    -> Text

  mapping
    :: Config
    -> SHM.HashMap (LogicalId (ResourceType r)) r

  all
    :: Config
    -> [ r ]
  all = SHM.elems . mapping

  -- allLogicalIds
  --   :: Config
  --   -> [ LogicalId (ResourceType r) ]
  -- allLogicalIds = SHM.keys . mapping

  getById
    :: Config
    -> LogicalId (ResourceType r)
    -> r
  getById config lid =
    fromMaybe
      (panic $ "Could not reference resource with logical id: " <> P.show lid)
      $ SHM.lookup lid $ mapping config

  logicalId
    :: Config
    -> r
    -> LogicalId (ResourceType r)
  logicalId _config r =
    LogicalId $ makeAlphaNumeric (name r) <> typeName r

  physicalId
    :: Config
    -> r
    -> PhysicalId (ResourceType r)


type LambdaId = LogicalId (ResourceType Lambda)
instance AwsResource Lambda where
  type ResourceType Lambda = 'LambdaResource

  typeName = const "Lambda"
  name = view lbdName
  mapping = view $ lbdConfig . lbdIdToLambda
  physicalId config r =
    PhysicalId $ makeAlphaNumeric (name r) `underscoreNamePrefixWith` config


type KfId = LogicalId (ResourceType Kf)
instance AwsResource Kf where
  type ResourceType Kf = 'KinesisFirehoseResource

  typeName = const "Kf"
  name = view kfName
  mapping = view $ kfConfig . kfIdToKf
  physicalId config r =
    PhysicalId $ makeAlphaNumeric (name r) `dotNamePrefixWith` config


type S3BucketId = LogicalId (ResourceType S3Bucket)
instance AwsResource S3Bucket where
  type ResourceType S3Bucket = 'S3BucketResource

  typeName = const "S3Bucket"
  name = view s3bName
  mapping = view $ s3Config . s3IdToBucket
  physicalId config r =
    PhysicalId $ makeAlphaNumeric (name r) `dotNamePrefixWith` config
