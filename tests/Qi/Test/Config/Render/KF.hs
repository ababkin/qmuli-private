{-# LANGUAGE OverloadedLists #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Qi.Test.Config.Render.KF where

import Data.Aeson
import           Control.Lens
import           Data.Default                          (def)
import           Protolude                             hiding (State, get, put,
                                                        runState)
import Polysemy
import Polysemy.State (runState)

import           Qi.AWS.Resource
import           Qi.AWS.S3
import qualified Qi.AWS.S3.Render                      as S3
import           Qi.AWS.KF
import qualified Qi.AWS.KF.Render                      as KF
import           Qi.AWS.Types
import           Qi.Config                             hiding (appName)
import qualified Qi.Program.Config.Ipret.State         as Config
import           Qi.Program.Config.Lang as Config
import           Qi.Test.Logger
import           Test.Tasty.Hspec
import Qi.Test.Util

import  Stratosphere
import  Stratosphere.Resources
import  Stratosphere.Resources.S3Bucket
import  Stratosphere.Resources.KinesisStream
import  Stratosphere.Values


spec :: Spec
spec = parallel $
  describe "ConfigEff" $ do
    let Right appName = mkAppName "testapp"
        bucketName = "mybucket"
        kfName = "mykf"

    describe "renders a CF stack spec with KF" $ do

      let config = runConfig $ do
                     bucketId <- Config.s3Bucket bucketName def
                     Config.s3BucketKf kfName bucketId

          runConfig configProgram =
                fst
              . run
              . runState (mkConfig appName)
              $ Config.run configProgram

      it "KF resource is rendered correctly" $ do
        let expectedBucketLogicalId = bucketName <> "S3Bucket"
            expectedRoleLogicalId = kfName <> "IAMRole"
            expectedKfLogicalId = kfName <> "KinesisFirehose"
            expectedKfPhysicalId = show appName <> "." <> kfName <> ".kinesis-firehose"
            expectedBucketArn = arnRef expectedBucketLogicalId
            expectedRoleArn = arnRef expectedRoleLogicalId
            arnRef id = Object [("Fn::GetAtt"
                                , Array [ String id
                                        , String "Arn"
                                        ]
                                )]

        case KF.toResources config of
          Resources [Resource kfLogicalId (ResourceProperties _ props) _ _ _ _ _ _] -> do

            kfLogicalId `shouldBe` expectedKfLogicalId

            withProps props $ \(propShouldBe, subProps) -> do
              "DeliveryStreamName" `propShouldBe` String expectedKfPhysicalId
              "DeliveryStreamType" `propShouldBe` String "DirectPut"

              withProps (subProps "S3DestinationConfiguration") $
                \(propShouldBe', subProps') -> do
                  "BucketARN" `propShouldBe'` expectedBucketArn

                  withProps (subProps' "BufferingHints") $
                    \(propShouldBe'', _subProps'') -> do
                      "IntervalInSeconds" `propShouldBe''` Number 60
                      "SizeInMBs" `propShouldBe''` Number 1
                  "CompressionFormat" `propShouldBe'` String "UNCOMPRESSED"
                  "Prefix" `propShouldBe'` String "mydata"
                  "RoleARN" `propShouldBe'` expectedRoleArn

          unexpected -> panic $ "unexpected number of resources created: " <> show unexpected
