{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Qi.Test.Config.Render.S3 where


import Data.Aeson
import           Control.Lens
import           Control.Monad.Freer
import           Control.Monad.Freer.State
import           Data.Default                          (def)
import qualified Data.HashMap.Strict                   as SHM
import           Protolude                             hiding (State, get, put,
                                                        runState)
import           Qi.AWS.Resource
import           Qi.AWS.S3
import qualified Qi.AWS.S3.Render                      as S3
import           Qi.AWS.Types
import           Qi.Config                             hiding (appName)
import qualified Qi.Program.Config.Ipret.State         as Config
import           Qi.Program.Config.Lang as Config
import           Qi.Test.Logger
import           Test.Tasty.Hspec

import  Stratosphere
import  Stratosphere.Resources
import  Stratosphere.Resources.S3Bucket  
import  Stratosphere.Values


spec :: Spec
spec = parallel $
  describe "ConfigEff" $ do
    let Right appName = mkAppName "testapp"
        bucketName = "mybucket"

    describe "renders a CF stack spec with S3 bucket" $ do

      let config = runConfig $ do
                     Config.s3Bucket bucketName def

          runConfig configProgram =
                snd
              . run
              . runState (mkConfig appName)
              $ Config.run configProgram

-- https://github.com/frontrowed/stratosphere/blob/master/library-gen/Stratosphere/Resources/S3Bucket.hs
--
      it "S3 bucket resource is rendered correctly" $ do
        -- https://github.com/frontrowed/stratosphere/blob/master/library-gen/Stratosphere/ResourceProperties/S3BucketS3KeyFilter.hs
        let expectedBucketLogicalId = bucketName <> "S3Bucket"
            expectedBucketPhysicalId = show appName <> "." <> bucketName <> ".s3-bucket"
            expectedNotificationConfig = Just (S3BucketNotificationConfiguration {
                                                  _s3BucketNotificationConfigurationLambdaConfigurations = Just []
                                                , _s3BucketNotificationConfigurationQueueConfigurations = Nothing
                                                , _s3BucketNotificationConfigurationTopicConfigurations = Nothing
                                                })



        case S3.toResources config of
          Resources [ Resource bucketLogicalId (ResourceProperties _type props) _ _ _ _ _ _ ] -> do
            bucketLogicalId `shouldBe` expectedBucketLogicalId

            let propShouldBe propKey expectedTextValue =
                  SHM.lookup propKey props `shouldBe` Just (String expectedTextValue)

            "BucketName" `propShouldBe` expectedBucketPhysicalId

            -- view sbBucketName bucket `shouldBe` Just (Literal expectedPhysicalId)
           
      -- https://github.com/frontrowed/stratosphere/blob/master/library-gen/Stratosphere/ResourceProperties/S3BucketNotificationConfiguration.hs
     
            -- view sbNotificationConfiguration bucket `shouldBe` expectedNotificationConfig

          _ -> panic "unexpected number of resources created"
