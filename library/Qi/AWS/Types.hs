{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Qi.AWS.Types ( AwsMode (..)
                    -- ^ Local stack simulation or the real cloud AWS resources
                    , AwsResourceType (..)
                    , AppName
                    -- ^ Name of an app (hide the constructor)
                    , mkAppName
                    -- ^ Smart constructor for app names
                    -- , ResourceName
                    -- -- ^ Name of a resource (hide the constructor)
                    -- , mkResourceName
                    -- -- ^ Smart constructor for resource names
                    , LogicalId
                    -- ^ Logical Id for a resource (hide the constructor)
                    , mkLogicalId
                    -- ^ Smart constructor for Logical Id
                    , PhysicalId
                    -- ^ Physical Id for a resource (hide the constructor)
                    , mkPhysicalId
                    , parseLambdaPhysicalId
                    , parseS3BucketPhysicalId
                    -- ^ Physical Id parser
                    , toLogicalId
                    -- ^ demote Physical Id to Logical Id
                    , toAppName
                    -- ^ extract app name from the Physical Id
                    , toPhysicalId
                    -- ^ promote Logical Id to Physical Id
                    , castLogicalIdResource
                    -- ^ cast Logical Id resource type to a different one (while retaining the same base resource identifier)
                    , ResourceExistence (..)
                    -- ^ Whether a resource already exists or needs to be created as part of the stack
                    )
                     where

import           Data.Aeson
import           Data.Hashable (Hashable)
import           Protolude hiding (show)
import qualified          Protolude as P
import qualified Data.Text          as T
import qualified GHC.Show (Show(..))


data AwsMode = RealDeal | LocalStack
  deriving Eq


data AwsResourceType =
    S3BucketResource
  | KinesisFirehoseResource
  | LambdaResource
  | LambdaPermissionResource
  | IamRoleResource
  | CwEventsRuleResource
  -- | IamPolicyResource
  deriving (Eq, Show)

data ResourceExistence = AlreadyExists | ShouldCreate
  deriving (Eq, Show)


newtype AppName = AppName Text
  deriving (Eq, Ord)
  deriving newtype (ToJSON, FromJSON)
instance Show AppName where
  show (AppName t) = toS t

mkAppName :: Text -> Either Text AppName
mkAppName t = Right $ AppName t -- TODO: restrict app names according to validation rules

-- newtype ResourceName (r :: AwsResourceType) = ResourceName Text
--   deriving Eq
--   deriving newtype (Show, ToJSON, FromJSON, Hashable)

-- mkResourceName :: Text -> Maybe (ResourceName rt)
-- mkResourceName t = Just $ ResourceName t -- TODO: restrict names according to resource type

-- The logical ID must be alphanumeric (A-Za-z0-9) and unique within the template.
-- Use the logical name to reference the resource in other parts of the template.
-- For example, if you want to map an Amazon Elastic Block Store volume to an Amazon EC2 instance,
-- you reference the logical IDs to associate the block stores with the instance.
newtype LogicalId (r :: AwsResourceType) = LogicalId Text
  deriving Eq
  deriving newtype (ToJSON, FromJSON, Hashable)
instance Show (LogicalId 'S3BucketResource) where
  show (LogicalId t) = toS t <> "S3Bucket"
instance Show (LogicalId 'LambdaResource) where
  show (LogicalId t) = toS t <> "Lambda"
instance Show (LogicalId 'LambdaPermissionResource) where
  show (LogicalId t) = toS t <> "LambdaPermission"
instance Show (LogicalId 'KinesisFirehoseResource) where
  show (LogicalId t) = toS t <> "KinesisFirehose"
instance Show (LogicalId 'IamRoleResource) where
  show (LogicalId t) = toS t <> "IAMRole"
instance Show (LogicalId 'CwEventsRuleResource) where
  show (LogicalId t) = toS t <> "CloudWatchEventsRule"
-- instance Show (LogicalId 'IamPolicyResource) where
  -- show (LogicalId t) = toS t <> "IAMPolicy"

-- TODO: I think I need to remove this
mkLogicalId :: Text -> Either Text (LogicalId r)
mkLogicalId t = Right $ LogicalId t -- TODO: restrict names according to resource type

-- This is pretty much just the logical id prefixed with the app name
data PhysicalId (rt :: AwsResourceType) = PhysicalId AppName Text
  deriving Eq
instance Show (PhysicalId 'S3BucketResource) where
  show (PhysicalId appName id) = P.show appName <> "." <> toS id <> "." <> "s3-bucket"
instance Show (PhysicalId 'LambdaResource) where
  show (PhysicalId appName id) = P.show appName <> "_" <> toS id <> "_" <> "lambda"
instance Show (PhysicalId 'LambdaPermissionResource) where
  show (PhysicalId appName id) = P.show appName <> "." <> toS id <> "." <> "lambda-permission"
instance Show (PhysicalId 'KinesisFirehoseResource) where
  show (PhysicalId appName id) = P.show appName <> "." <> toS id <> "." <> "kinesis-firehose"
instance Show (PhysicalId 'IamRoleResource) where
  show (PhysicalId appName id) = P.show appName <> "_" <> toS id <> "_" <> "iam-role"
instance Show (PhysicalId 'CwEventsRuleResource) where
  show (PhysicalId appName id) = P.show appName <> "_" <> toS id <> "_" <> "cloud-watch-events-rule"
-- instance Show (PhysicalId 'IamPolicyResource) where
  -- show (PhysicalId appName id) = P.show appName <> "_" <> toS id <> "_" <> "iam-policy"

mkPhysicalId :: AppName -> Text -> Either Text (PhysicalId rt)
mkPhysicalId appName t = Right $ PhysicalId appName t
-- TODO: restrict names according to resource type
--
parseS3BucketPhysicalId :: Text -> Either Text (PhysicalId 'S3BucketResource)
parseS3BucketPhysicalId t = parsePhysicalId "s3-bucket" '.' t

parseLambdaPhysicalId :: Text -> Either Text (PhysicalId 'LambdaResource)
parseLambdaPhysicalId t = parsePhysicalId "lambda" '_' t

parsePhysicalId :: Text -> Char -> Text -> Either Text (PhysicalId r)
parsePhysicalId resourceSuffix appNameSeparator t =
  if resourceSuffix `T.isSuffixOf` t
    then physicalId
    else Left $ "The Physical Id suffix is incorrect, saw: " <> P.show t <>
                  ", expected to see the following suffix: " <> P.show resourceSuffix

  where
    appName = mkAppName $ T.take appNamePrefixLength t
    physicalId = (`mkPhysicalId` (dropResourceTypeSuffix $ T.drop (appNamePrefixLength + 1) t)) =<< appName
    appNamePrefixLength = T.length (T.takeWhile (/=appNameSeparator) t)
    dropResourceTypeSuffix = T.reverse . T.drop (T.length resourceSuffix + 1) . T.reverse

-- extract app name
toAppName :: PhysicalId rt -> AppName
toAppName (PhysicalId appName _id) = appName

-- demote the Id
toLogicalId :: PhysicalId rt -> LogicalId rt
toLogicalId (PhysicalId _appName id) = LogicalId id

-- promote the Id
toPhysicalId :: AppName -> LogicalId rt -> PhysicalId rt
toPhysicalId appName (LogicalId id) = PhysicalId appName id

castLogicalIdResource
  :: LogicalId (a :: AwsResourceType)
  -> LogicalId (b :: AwsResourceType)
castLogicalIdResource (LogicalId id) = LogicalId id
