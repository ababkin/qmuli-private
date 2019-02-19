{-# LANGUAGE OverloadedLists #-}

module Qi.AWS.Lambda.Render (toResources) where

import           Protolude             hiding (all)
import qualified Qi.AWS.IAMRole.Render as Role
import           Qi.AWS.Resource
import           Qi.AWS.Types
import           Qi.AWS.Lambda
import           Qi.Config
import           Stratosphere


toResources :: Config -> Resources
toResources config@Config{ _appName } = Resources $ map toLambdaResource lbds
  where
    lbds :: [ (LogicalId 'LambdaResource, Lambda) ] = all config

    toLambdaResource (lbdLogicalId, lbd) = (
      resource (show lbdLogicalId) $
        LambdaFunctionProperties $
        lambdaFunction
          lbdCode
          "index.handler"
          (GetAtt Role.lambdaBasicExecutionIAMRoleLogicalName "Arn")
          (Literal $ OtherRuntime "provided")
        & lfFunctionName  ?~ Literal (show $ toPhysicalId _appName lbdLogicalId)
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
        lambdaS3Bucket = Literal $ show _appName <> ".app"

        lambdaS3Object :: Val Text
        lambdaS3Object = "lambda.zip"
