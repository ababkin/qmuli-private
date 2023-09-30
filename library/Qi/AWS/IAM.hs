{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Qi.AWS.IAM where

import Data.Aeson
import Data.Default (Default, def)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as SHM
import GHC.Show (Show (..))
import Protolude as P
import Qi.AWS.ARN
import Qi.AWS.Renderable
import Qi.AWS.Service
import Qi.AWS.Types
import Stratosphere
import qualified Stratosphere as S
import Stratosphere.IAM.Role
import Stratosphere.IAM.Policy

data IamRole = IamRole
  { principalArn :: Arn
  }
  deriving (Eq, Show)

instance AwsResource IamRole where
  type ResourceType IamRole = 'IamRoleResource

data IamConfig = IamConfig
  { _idToRole :: HashMap RoleId IamRole
  }
  deriving (Eq, Show)

instance Default IamConfig where
  def =
    IamConfig
      { _idToRole = SHM.empty
      }

instance Renderable IamRole where
  render appName (lid, IamRole {principalArn}) =
    S.resource (P.show lid) role

    where
      role :: Role
      role = mkRole rolePolicyDocumentObject
        & set @"Policies" [executePolicy]
        & set @"RoleName" (Literal . P.show $ toPhysicalId appName lid)
        & set @"Path" (Literal "/")

      -- reqs = [ "myEventLambdaLambda" ]

      rolePolicyDocumentObject =
        [ ("Version", "2012-10-17"),
          ("Statement", rolePolicyStatement)
        ]

      rolePolicyStatement =
        object
          [ ("Effect", "Allow"),
            ("Principal", principal),
            ("Action", "sts:AssumeRole")
          ]

      -- NOTE: it seems like we cannot use a specific lambda's ARN here. This may be ok
      -- since we attach a particular role to a particular lambda anyway
      -- (and we have one role per lambda)
      -- principal = object [ ("AWS", String "arn:aws:lambda:us-east-1:445506728970:function:echo-access" )] -- toJSON principalArn) ]
      -- principal = object [ ("AWS", toJSON principalArn) ]
      principal = object [("Service", String . toUrl $ service principalArn)]
      -- [ ("Service", "lambda.amazonaws.com")]

      executePolicy =
        mkPolicyProperty policyDocument policyName

        where
          policyDocument = 
            [ "Version" .= String "2012-10-17",
              "Statement" .= execStatement
            ]

          policyName = Literal $ P.show lid <> "Policy"

          execStatement =
            object
              [ ("Effect", "Allow"),
                ("Action", actions),
                ("Resource", "*")
              ]

          actions =
            Array
              [ "logs:CreateLogGroup",
                "logs:DescribeLogGroups",
                "logs:CreateLogStream",
                "logs:DescribeLogStreams",
                "logs:PutLogEvents",
                "s3:GetObject",
                "s3:GetObjectAcl",
                "s3:GetObjectTagging",
                "s3:GetObjectVersion",
                "s3:GetObjectVersionAcl",
                "s3:GetObjectVersionTagging",
                "s3:PutObject",
                "s3:PutObjectAcl",
                "s3:PutObjectTagging",
                "s3:PutObjectVersionAcl",
                "s3:PutObjectVersionTagging",
                "s3:DeleteObject",
                "s3:DeleteObjectTagging",
                "s3:DeleteObjectVersion",
                "s3:DeleteObjectVersionTagging",
                "s3:RestoreObject",
                "s3:CreateBucket",
                "s3:DeleteBucket",
                "s3:GetBucketLocation",
                "s3:ListBucket",
                "s3:ListBucketVersions",
                "s3:ListAllMyBuckets",
                "s3:ListBucketMultipartUploads",
                "s3:ListMultipartUploadParts",
                "s3:AbortMultipartUpload",
                "dynamodb:Scan",
                "dynamodb:Query",
                "dynamodb:GetItem",
                "dynamodb:PutItem",
                "dynamodb:DeleteItem",
                "dynamodb:GetRecords",
                "dynamodb:GetShardIterator",
                "dynamodb:DescribeStream",
                "dynamodb:ListStreams",
                "cognito-idp:CreateUserPool",
                "cognito-idp:DeleteUserPool",
                "cognito-idp:CreateUserPoolClient",
                "cognito-identity:CreateIdentityPool",
                "cognito-identity:DeleteIdentityPool",
                "cognito-identity:SetIdentityPoolRoles",
                "iam:CreateRole",
                "iam:DeleteRole",
                "iam:PassRole",
                "iam:PutRolePolicy",
                "iam:DeleteRolePolicy",
                "lambda:InvokeFunction",
                "lex:*",
                "firehose:*"
              ]
