{-# LANGUAGE DeriveAnyClass #-}

-- | ARN
-- https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html
-- Amazon Resource Names (ARNs) uniquely identify AWS resources. We require an ARN when you need
-- to specify a resource unambiguously across all of AWS, such as in IAM policies,
-- Amazon Relational Database Service (Amazon RDS) tags, and API calls.
module Qi.AWS.ARN
  ( Arn (..),
    ToArn (..),
  )
where

import Control.Lens
import Control.Monad.Fail
import Data.Aeson
import qualified Data.Text as T
import GHC.Show (Show (..))
import Protolude hiding (show)
import qualified Protolude as P
import Qi.AWS.Service
import Qi.AWS.Types

data Arn = Arn
  { service :: Service,
    region :: Text,
    resource :: Text
  }
  deriving (Eq)

instance Show Arn where
  show Arn {service, region, resource} =
    toS $
      mconcat
        [ "arn",
          ":",
          "aws",
          ":",
          toNamespace service,
          ":",
          region,
          ":",
          "445506728970", -- account id TODO: dont hardcode this
          ":",
          resource
        ]

instance ToJSON Arn where
  toJSON = String . P.show

instance FromJSON Arn where
  parseJSON = withText "Arn" $ \t ->
    case T.splitOn ":" t of
      "arn" : partition' : service' : region' : resource' ->
        case Arn <$> fromNamespace service'
          <*> pure region'
          <*> Just (mconcat $ intersperse ":" resource') of
          Just arn -> pure arn
          Nothing -> fail $ "could not parse Arn: " <> show t
      _ -> fail "could not parse Arn"

-- NOTE: for referring to ARNs while rendering CF template use this:
-- `(GetAtt (show logicalId) "Arn") `
class ToArn a where
  toArn :: a -> AppName -> Arn

instance ToArn S3BucketId where
  toArn id appName =
    Arn
      { service = S3,
        region = "",
        resource = P.show $ toPhysicalId appName id
      }

instance ToArn LambdaId where
  toArn id appName =
    Arn
      { service = Lambda,
        region = "us-east-1",
        resource = "function" <> ":" <> P.show (toPhysicalId appName id)
      }
