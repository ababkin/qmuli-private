{-# LANGUAGE OverloadedLists #-}

module Qi.AWS.IAMRole.Render (
    toResources
  ) where

import           Data.Aeson  
import           Protolude hiding (all)
import           Qi.AWS.Resource
import           Qi.AWS.Types
import           Qi.AWS.IAM
import           Qi.AWS.ARN
import           Qi.Config
import           Stratosphere


toResources
  :: Config
  -> Resources
toResources config@Config{ _appName } = Resources $ toResource <$> roles

  where
    roles = all config

    toResource (lid, IamRole{ principalArn }) =
      let res = resource (show lid) $
                  iamRole rolePolicyDocumentObject
                  & iamrPolicies ?~ [ executePolicy ]
                  & iamrRoleName ?~ Literal (show $ toPhysicalId _appName lid)
                  & iamrPath ?~ "/"
      in res -- & resourceDependsOn ?~ reqs


      where
        -- reqs = [ "myEventLambdaLambda" ]

        rolePolicyDocumentObject =
          [ ("Version", "2012-10-17")
          , ("Statement", rolePolicyStatement)
          ]

        rolePolicyStatement = object
          [ ("Effect", "Allow")
          , ("Principal", principal)
          , ("Action", "sts:AssumeRole")
          ]

        -- NOTE: it seems like we cannot use a specific lambda's ARN here. This may be ok
        -- since we attach a particular role to a particular lambda anyway
        -- (and we have one role per lambda)
        -- principal = object [ ("AWS", String "arn:aws:lambda:us-east-1:445506728970:function:echo-access" )] -- toJSON principalArn) ]
        -- principal = object [ ("AWS", toJSON principalArn) ]
        principal = object [ ("Service", String $ toToken (service principalArn) <> ".amazonaws.com") ]
          -- [ ("Service", "lambda.amazonaws.com")]
          -- , ("Service", "firehose.amazonaws.com") ]

        executePolicy = iamRolePolicy
                            [ ("Version", "2012-10-17")
                            , ("Statement", execStatement)
                            ] $
                            Literal $ show lid <> "Policy"

          where
            execStatement = object
              [ ("Effect", "Allow")
              , ("Action", actions)
              , ("Resource", "*")
              ]

            actions = Array
              [ "logs:CreateLogGroup"
              , "logs:DescribeLogGroups"
              , "logs:CreateLogStream"
              , "logs:DescribeLogStreams"
              , "logs:PutLogEvents"

              , "s3:GetObject"
              , "s3:GetObjectAcl"
              , "s3:GetObjectTagging"
              , "s3:GetObjectVersion"
              , "s3:GetObjectVersionAcl"
              , "s3:GetObjectVersionTagging"
              , "s3:PutObject"
              , "s3:PutObjectAcl"
              , "s3:PutObjectTagging"
              , "s3:PutObjectVersionAcl"
              , "s3:PutObjectVersionTagging"
              , "s3:DeleteObject"
              , "s3:DeleteObjectTagging"
              , "s3:DeleteObjectVersion"
              , "s3:DeleteObjectVersionTagging"
              , "s3:RestoreObject"
              , "s3:CreateBucket"
              , "s3:DeleteBucket"
              , "s3:GetBucketLocation"
              , "s3:ListBucket"
              , "s3:ListBucketVersions"
              , "s3:ListAllMyBuckets"
              , "s3:ListBucketMultipartUploads"
              , "s3:ListMultipartUploadParts"
              , "s3:AbortMultipartUpload"

              , "dynamodb:Scan"
              , "dynamodb:Query"
              , "dynamodb:GetItem"
              , "dynamodb:PutItem"
              , "dynamodb:DeleteItem"
              , "dynamodb:GetRecords"
              , "dynamodb:GetShardIterator"
              , "dynamodb:DescribeStream"
              , "dynamodb:ListStreams"

              , "cognito-idp:CreateUserPool"
              , "cognito-idp:DeleteUserPool"
              , "cognito-idp:CreateUserPoolClient"

              , "cognito-identity:CreateIdentityPool"
              , "cognito-identity:DeleteIdentityPool"
              , "cognito-identity:SetIdentityPoolRoles"

              , "iam:CreateRole"
              , "iam:DeleteRole"
              , "iam:PassRole"
              , "iam:PutRolePolicy"
              , "iam:DeleteRolePolicy"

              , "lambda:InvokeFunction"

              , "lex:*"

              , "firehose:*"
              ]


