-- |
module Qi.AWS.Lambda.Permission where

import Data.Aeson (FromJSON, ToJSON)
import Protolude
-- hiding (lfRole, LambdaFunction)

import Qi.AWS.Renderable
import Qi.AWS.Service
import Qi.AWS.Types
import Stratosphere (Val (..))
import qualified Stratosphere as S

-- this creates a permission for the source (e.g. S3 bucket, another Lambda, etc) to call the Lambda
--
data LambdaPermission = LambdaPermission
  { principal :: Service, -- what service can call the lambda function
    functionId :: LambdaId
  }
  deriving (Eq, Show)

instance AwsResource LambdaPermission where
  type ResourceType LambdaPermission = 'LambdaPermissionResource

instance Renderable LambdaPermission where
  render appName (lid, LambdaPermission {principal, functionId}) =
    S.resource (show lid) $
      S.lambdaPermission
        "lambda:*"
        (GetAtt (show functionId) "Arn")
        (Literal . toUrl $ principal)
