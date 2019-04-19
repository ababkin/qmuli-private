-- | ARN
-- https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html
-- Amazon Resource Names (ARNs) uniquely identify AWS resources. We require an ARN when you need
-- to specify a resource unambiguously across all of AWS, such as in IAM policies,
-- Amazon Relational Database Service (Amazon RDS) tags, and API calls.

{-# LANGUAGE DeriveAnyClass #-}

module Qi.AWS.ARN (
    Arn
  , ToArn(..)
  -- , mkS3ObjectArn
  ) where

import           Control.Monad.Fail
import           Data.Aeson
import qualified Data.Text          as T
import           Protolude hiding (show)
import qualified          Protolude as P
import           Qi.AWS.S3
import           Qi.AWS.Types
import GHC.Show (Show(..))
import           Control.Lens


class ArnToken a where
  toToken :: a -> Text
  fromToken :: Text -> Maybe a

instance ArnToken Text where
  toToken = identity
  fromToken = Just

data ArnPartition = AwsPartition
  deriving (Eq, Show)
instance ArnToken ArnPartition where
  toToken AwsPartition = "aws"
  fromToken "aws" = Just AwsPartition
  fromToken _     = Nothing

data Service =
    S3
  | KF
  | Dynamo
  | Lambda
  deriving (Eq, Show)

instance ArnToken Service where
        toToken S3     = "s3"
        toToken KF     = "firehose"
        toToken Dynamo = "dynamodb"
        toToken Lambda = "lambda"

        fromToken "s3"       = Just S3
        fromToken "firehose" = Just KF
        fromToken "dynamodb" = Just Dynamo
        fromToken "lambda"   = Just Lambda
        fromToken _          = Nothing

data Arn = Arn
  { partition :: ArnPartition
  , service   :: Service
  , region    :: Text
  , resource  :: Text
  }
  deriving (Eq)

instance Show Arn where
  show Arn{ partition, service, region, resource } = toS $ mconcat
        [ toToken partition
        , ":"
        , toToken service
        , ":"
        , toToken region
        , ":"
        , toToken resource
        ]
instance ToJSON Arn where
  toJSON = String . P.show
instance FromJSON Arn where
  parseJSON = withText "Arn" $ \t ->
    case T.splitOn ":" t of
      partition' : service' : region' : resource' ->
        case Arn <$> fromToken partition'
                <*> fromToken service'
                <*> fromToken region'
                <*> Just ( mconcat $ intersperse ":" resource' ) of
          Just arn -> pure arn
          Nothing  -> fail "could not parse Arn"
      _ -> fail "could not parse Arn"



-- arn:aws:s3:::my_corporate_bucket/exampleobject.png
-- mkS3ObjectArn
--   :: PhysicalId 'S3BucketResource
--   -> S3Key
--   -> Arn
-- mkS3ObjectArn s3BucketPhysicalId (S3Key key) = Arn {
--     partition = AwsPartition
--   , service = S3
--   , region = ""
--   , resource = arnRes
--   }
--   where
--     arnRes = P.show s3BucketPhysicalId <> "/" <> key

class ToArn a where
  toArn :: a -> AppName -> Arn

instance ToArn S3BucketId where
  toArn id appName = Arn {
      partition = AwsPartition
    , service = S3
    , region = ""
    , resource = P.show $ toPhysicalId appName id
    }

instance ToArn S3Object where
  toArn s3obj appName = Arn {
      partition = AwsPartition
    , service = S3
    , region = ""
    , resource = arnRes
    }
    where
      bucketId =  s3obj ^. s3oBucketId
      objKey =  s3obj ^. s3oKey
      arnRes = P.show (toPhysicalId appName bucketId) <> "/" <> P.show objKey
