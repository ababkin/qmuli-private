{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Qi.Test.Config.Render where

import Data.Aeson
import           Control.Lens
import           Data.Default                          (def)
import qualified Data.HashMap.Strict                   as SHM
import           Protolude                             hiding (State, get, put,
                                                        runState)
import Polysemy
import Polysemy.State (runState)

import           Qi.AWS.Lambda
import qualified Qi.AWS.Lambda.Render                  as Lambda
import qualified Qi.AWS.LambdaPermission.Render        as LambdaPermission
import           Qi.AWS.Resource
import           Qi.AWS.S3
import qualified Qi.AWS.S3.Render                      as S3
import           Qi.AWS.Types
import           Qi.Config                             hiding (appName)
import qualified Qi.Program.Config.Ipret.State         as Config
import           Qi.Program.Config.Lang
import           Qi.Test.Logger
import           Test.Tasty.Hspec

import qualified Stratosphere                          as S
import qualified Stratosphere.Resources                as S
import qualified Stratosphere.Resources.LambdaFunction as S
import qualified Stratosphere.Resources.S3Bucket       as S
import qualified Stratosphere.Values                   as S



-- https://github.com/frontrowed/stratosphere/blob/34827b93db58495a60896b4cb132353bc0734e5c/library-gen/Stratosphere/Resources.hs
--
--data Resource =
  -- Resource
  -- { _resourceName :: T.Text
  -- , _resourceProperties :: ResourceProperties
  -- , _resourceDeletionPolicy :: Maybe DeletionPolicy
  -- , _resourceCreationPolicy :: Maybe CreationPolicy
  -- , _resourceUpdatePolicy :: Maybe UpdatePolicy
  -- , _resourceDependsOn :: Maybe [T.Text]
  -- , _resourceMetadata :: Maybe Object
  -- } deriving (Show, Eq)
 
spec :: Spec
spec = parallel $
  describe "ConfigEff" $ do
    let Right appName = mkAppName "testapp"
        bucketName = "mybucket"
        lambdaName = "mylambda"
        lambdaProgram _ = pure "blah"

    describe "inserts an S3 bucket, lambda into the S3 config and attaches them correctly" $ do

      let expectedLambdaLogicalId = lambdaName <> "LambdaFunction"
          config = runConfig $ do
                        bucketId <- s3Bucket bucketName def
                        void $ s3BucketLambda lambdaName bucketId lambdaProgram $
                                def & lfpMemorySize .~ M1536

          runConfig configProgram =
                fst
              . run
              . runState (mkConfig appName)
              $ Config.run configProgram

-- https://github.com/frontrowed/stratosphere/blob/master/library-gen/Stratosphere/Resources/S3Bucket.hs
--
      it "S3 bucket resource is rendered correctly" $ do
        -- https://github.com/frontrowed/stratosphere/blob/master/library-gen/Stratosphere/ResourceProperties/S3BucketS3KeyFilter.hs
        let expectedBucketLogicalId = bucketName <> "S3Bucket"
            expectedFilters  = Nothing -- Just (S.S3BucketNotificationFilter (S.S3BucketS3KeyFilter []))
            expectedBucketPhysicalId = show appName <> "." <> bucketName <> ".s3-bucket"

        case S3.toResources config of
          S.Resources [ S.Resource bucketLogicalId (S.ResourceProperties _type props) _ _ _ _ _ _ ] -> do
            bucketLogicalId `shouldBe` expectedBucketLogicalId

            -- let propShouldBe propKey expectedTextValue =
            --       SHM.lookup propKey bucketProps `shouldBe` Just (String expectedTextValue)

            -- "BucketName" `propShouldBe` expectedBucketPhysicalId

          _ -> panic "unexpected number of resources created"

     
      -- TODO
{-
      -- https://github.com/frontrowed/stratosphere/blob/master/library-gen/Stratosphere/ResourceProperties/S3BucketNotificationConfiguration.hs
      --
            let Just ( S.S3BucketNotificationConfiguration (Just [lbdNotifyConfig]) _ _ ) =
                  view S.sbNotificationConfiguration bucket

      -- https://github.com/frontrowed/stratosphere/blob/master/library-gen/Stratosphere/ResourceProperties/S3BucketLambdaConfiguration.hs
      --
            view S.sblcEvent lbdNotifyConfig `shouldBe` S.Literal "s3:ObjectCreated:*"
            view S.sblcFilter lbdNotifyConfig `shouldBe` expectedFilters
            view S.sblcFunction lbdNotifyConfig `shouldBe` S.GetAtt expectedLambdaLogicalId "Arn"

          _ -> panic "unexpected number of resources created"

-}
      it "Lambda resource is rendered correctly" $ do
        case Lambda.toResources config of
          S.Resources [ S.Resource lambdaId (S.ResourceProperties _ _lbd) _ _ _ _ _ _ ] -> do
            lambdaId `shouldBe` expectedLambdaLogicalId

          _ -> panic "unexpected number of resources created"

      it "LambdaPermission resource is rendered correctly" $ do
        let expectedLogicalRoleId = lambdaName <> "LambdaPermission"
        case LambdaPermission.toResources config of
          S.Resources [ S.Resource roleId _ _ _ _ _ _ _ ] -> do
            roleId `shouldBe` expectedLogicalRoleId

          _ -> panic "unexpected number of resources created"
