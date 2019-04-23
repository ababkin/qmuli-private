-- | ARN
-- https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html
-- Amazon Resource Names (ARNs) uniquely identify AWS resources. We require an ARN when you need
-- to specify a resource unambiguously across all of AWS, such as in IAM policies,
-- Amazon Relational Database Service (Amazon RDS) tags, and API calls.

{-# LANGUAGE DeriveAnyClass #-}

module Qi.AWS.ARN (
    Arn(service)
  , ToArn(..)
  , toPrincipal
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


type KfId = LogicalId 'KfStreamResource
type LambdaFunctionId = LogicalId 'LambdaFunctionResource

data Arn = Arn
  { service   :: Service
  , region    :: Text
  , resource  :: Text
  }
  deriving (Eq)

instance Show Arn where
  show Arn{ service, region, resource } = toS $ mconcat
        [ "arn"
        , ":"
        , "aws"
        , ":"
        , toNamespace service
        , ":"
        , region
        , ":"
        , "445506728970" -- account id TODO: dont hardcode this
        , ":"
        , resource
        ]
instance ToJSON Arn where
  toJSON = String . P.show
instance FromJSON Arn where
  parseJSON = withText "Arn" $ \t ->
    case T.splitOn ":" t of
      "arn" : partition' : service' : region' : resource' ->
        case Arn <$> fromNamespace service'
                 <*> pure region'
                 <*> Just ( mconcat $ intersperse ":" resource' ) of
          Just arn -> pure arn
          Nothing  -> fail $ "could not parse Arn: " <> show t
      _ -> fail "could not parse Arn"



-- NOTE: for referring to ARNs while rendering CF template use this:
-- `(GetAtt (show logicalId) "Arn") `
class ToArn a where
  toArn :: a -> AppName -> Arn

instance ToArn S3BucketId where
  toArn id appName = Arn {
      service = S3
    , region  = ""
    , resource = P.show $ toPhysicalId appName id
    }

instance ToArn S3Object where
  toArn s3obj appName = Arn {
      service = S3
    , region  = ""
    , resource = arnRes
    }
    where
      bucketId = s3obj ^. s3oBucketId
      objKey = s3obj ^. s3oKey
      arnRes = P.show (toPhysicalId appName bucketId) <> "/" <> P.show objKey

instance ToArn LambdaFunctionId where
  toArn id appName = Arn {
      service = Lambda
    , region  = "us-east-1"
    , resource = "function" <> ":" <> P.show (toPhysicalId appName id)
    }

instance ToArn KfId where
  toArn id appName = Arn {
      service = KinesisFirehose
    , region  = "us-east-1"
    , resource = "deliverystream" <> ":" <> P.show (toPhysicalId appName id)
    }
