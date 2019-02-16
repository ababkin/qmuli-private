{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}

module Qi.AWS.Types ( AwsMode (..)
                    , AwsResourceType (..)
                    , LogicalId (..)
                    , PhysicalId (..)
                    , ResourceExistence (..)
                    )
                     where

import           Data.Aeson
import           Data.Hashable (Hashable)
import           Protolude


data AwsMode = RealDeal | LocalStack
  deriving Eq


data AwsResourceType =
    S3BucketResource
  | KinesisFirehoseResource
  | LambdaResource
  deriving (Eq, Show)

data ResourceExistence = AlreadyExists | ShouldCreate
  deriving (Eq, Show)

-- The logical ID must be alphanumeric (A-Za-z0-9) and unique within the template.
-- Use the logical name to reference the resource in other parts of the template.
-- For example, if you want to map an Amazon Elastic Block Store volume to an Amazon EC2 instance,
-- you reference the logical IDs to associate the block stores with the instance.
newtype LogicalId (r :: AwsResourceType) = LogicalId { unLogicalId :: Text }
  deriving Eq
  deriving newtype (Show, ToJSON, FromJSON, Hashable)

newtype PhysicalId (r :: AwsResourceType) = PhysicalId { unPhysicalId :: Text }
  deriving Eq
  deriving newtype (Show, ToJSON, FromJSON)
