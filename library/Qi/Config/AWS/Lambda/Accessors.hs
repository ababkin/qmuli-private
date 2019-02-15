module Qi.Config.AWS.Lambda.Accessors where

import           Protolude
import           Qi.AWS.Resource
import           Qi.Config.AWS.Lambda


getPermissionLogicalId
  :: Lambda
  -> Text
getPermissionLogicalId lbd =
  name lbd <> "LambdaPermission"
