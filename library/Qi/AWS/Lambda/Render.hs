{-# LANGUAGE OverloadedLists #-}

module Qi.AWS.Lambda.Render (toResources) where

import           Protolude             hiding (all)
import           Qi.AWS.Resource
import           Qi.AWS.Types
import           Qi.AWS.Lambda
import           Qi.Config
import           Stratosphere hiding (lfRole, LambdaFunction)


toResources :: Config -> Resources
toResources config@Config{ _appName } = Resources $ map toLambdaResource lbds
  where
    lbds :: [ (LambdaId, LambdaFunction) ] = all config

    toLambdaResource (lbdLogicalId, lbd@LambdaFunction{ roleId, profile }) = (
      resource (show lbdLogicalId) $
        lambdaFunction
          lbdCode
          "index.handler"
          (GetAtt (show roleId) "Arn")
          (Literal $ OtherRuntime "provided")
        & lfFunctionName  ?~ Literal (show $ toPhysicalId _appName lbdLogicalId)
        & lfMemorySize    ?~ Literal memorySize
        & lfTimeout       ?~ Literal timeOut
      )

      where
        memorySize  = fromIntegral . fromEnum $ profile ^. lfpMemorySize
        timeOut     = fromIntegral $ profile ^. lfpTimeoutSeconds

        lbdCode :: LambdaFunctionCode
        lbdCode = lambdaFunctionCode
          & lfcS3Bucket ?~ lambdaS3Bucket
          & lfcS3Key    ?~ lambdaS3Object

        lambdaS3Bucket :: Val Text
        lambdaS3Bucket = Literal $ show _appName <> ".app"

        lambdaS3Object :: Val Text
        lambdaS3Object = "lambda.zip"
