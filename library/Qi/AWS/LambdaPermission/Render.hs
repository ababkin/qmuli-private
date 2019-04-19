{-# LANGUAGE OverloadedLists #-}

module Qi.AWS.LambdaPermission.Render (toResources) where

import           Protolude             hiding (all)
import qualified Qi.AWS.IAMRole.Render as Role
import           Qi.AWS.Resource
import           Qi.AWS.Types
import           Qi.AWS.Lambda
import           Qi.Config
import           Stratosphere


toResources :: Config -> Resources
toResources config@Config{ _appName } = Resources $ map toLambdaPermissionResource lbds
  where
    lbds :: [ (LogicalId 'LambdaResource, Lambda) ] = all config

    toLambdaPermissionResource (lbdLogicalId, lbd) =
          resource (show lbdPermissionLogicalId) $
            lambdaPermission
              "lambda:*"
              (GetAtt (show lbdLogicalId) "Arn")
              principal
          where
            lbdPermissionLogicalId :: LogicalId 'LambdaPermissionResource = castLogicalIdResource lbdLogicalId

            principal = case lbd of
              GenericLambda{}  -> "lambda.amazonaws.com"
              S3BucketLambda{} -> "s3.amazonaws.com"
