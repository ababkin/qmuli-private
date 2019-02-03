-- | ARN
-- https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html
-- Amazon Resource Names (ARNs) uniquely identify AWS resources. We require an ARN when you need
-- to specify a resource unambiguously across all of AWS, such as in IAM policies,
-- Amazon Relational Database Service (Amazon RDS) tags, and API calls.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Qi.AWS.ARN (
    Arn
  , mkS3ObjectArn
  ) where

import           Control.Monad.Fail
import           Data.Aeson
import qualified Data.Text          as T
import           Network.AWS        (Logger)
import           Protolude
import           Qi.Config.AWS.S3

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
  deriving (Eq, Show)

instance ToJSON Arn where
  toJSON Arn{ partition, service, region, resource } = String $ mconcat
        [ toToken partition
        , ":"
        , toToken service
        , ":"
        , toToken region
        , ":"
        , toToken resource
        ]
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
mkS3ObjectArn
  :: S3Bucket
  -> S3Key
  -> Arn
mkS3ObjectArn S3Bucket{ _s3bName } (S3Key key) = Arn {
    partition = AwsPartition
  , service = S3
  , region = ""
  , resource = arnRes
  }
  where
    arnRes = _s3bName <> "/" <> key
