{-# LANGUAGE OverloadedLists #-}

module Qi.AWS.LambdaPermission.Render (toResources) where

import           Protolude             hiding (all)
import qualified Qi.AWS.IAMRole.Render as Role
import           Qi.AWS.Resource
import           Qi.AWS.Types
import           Qi.AWS.Lambda
import           Qi.Config
import           Stratosphere hiding (LambdaFunction)


-- this creates a permission for the source (e.g. S3 bucket, another Lambda, etc) to call the Lambda

toResources :: Config -> Resources
toResources config@Config{ _appName } = Resources $ map toLambdaPermissionResource lbds
  where
    lbds :: [ (LambdaId, LambdaFunction) ] = all config

    toLambdaPermissionResource (lid, lbd@LambdaFunction{ principal }) =
      resource (show lbdPermissionLogicalId) $
        lambdaPermission
          "lambda:*"
          (GetAtt (show lid) "Arn")
          (Literal . toUrl $ principal)
      where
        lbdPermissionLogicalId :: LogicalId 'LambdaPermissionResource = castLogicalIdResource lid
