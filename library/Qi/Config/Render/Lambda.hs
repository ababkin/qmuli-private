{-# LANGUAGE OverloadedLists #-}

module Qi.Config.Render.Lambda (toResources) where

import           Protolude                      hiding (all)
import           Qi.AWS.Resource
import           Qi.AWS.Types
import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda           hiding (lbdName)
import           Qi.Config.AWS.Lambda.Accessors
import qualified Qi.Config.Render.Role          as Role
import           Stratosphere


toResources :: Config -> Resources
toResources config = foldMap toAllLambdaResources $ all config
  where
    toAllLambdaResources :: Lambda -> Resources
    toAllLambdaResources lbd = Resources $ [lambdaPermissionResource, lambdaResource]

      where
        lbdLogicalId = logicalId config lbd
        lbdPermissionLogicalId = getPermissionLogicalId lbd

        lambdaPermissionResource =
          resource lbdPermissionLogicalId $
            LambdaPermissionProperties $
            lambdaPermission
              "lambda:*"
              (GetAtt (unLogicalId lbdLogicalId) "Arn")
              principal
          where
            principal = case lbd of
              GenericLambda{}  -> "lambda.amazonaws.com"
              S3BucketLambda{} -> "s3.amazonaws.com"

        lambdaResource = (
          resource (unLogicalId lbdLogicalId) $
            LambdaFunctionProperties $
            lambdaFunction
              lbdCode
              "index.handler"
              (GetAtt Role.lambdaBasicExecutionIAMRoleLogicalName "Arn")
              (Literal $ OtherRuntime "provided")
            & lfFunctionName  ?~ Literal (unPhysicalId $ physicalId config lbd)
            & lfMemorySize    ?~ Literal memorySize
            & lfTimeout       ?~ Literal timeOut
          )

          where
            memorySize  = fromIntegral . fromEnum $ lbd ^. lbdProfile . lpMemorySize
            timeOut     = fromIntegral $ lbd ^. lbdProfile . lpTimeoutSeconds

            lbdCode :: LambdaFunctionCode
            lbdCode = lambdaFunctionCode
              & lfcS3Bucket ?~ lambdaS3Bucket
              & lfcS3Key    ?~ lambdaS3Object

            lambdaS3Bucket :: Val Text
            lambdaS3Bucket = Literal $ (config ^. namePrefix) <> ".app"

            lambdaS3Object :: Val Text
            lambdaS3Object = "lambda.zip"

