module Qi.AWS.Lambda.Accessors where

import           Protolude
import           Qi.AWS.Lambda
import           Qi.AWS.Resource


getPermissionLogicalId
  :: Lambda
  -> Text
getPermissionLogicalId lbd =
  name lbd <> "LambdaPermission"
