{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Qi.Test.Config.Render.KF where

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
import           Qi.AWS.KF
import qualified Qi.AWS.KF.Render                      as KF
import           Qi.AWS.Types
import           Qi.Config                             hiding (appName)
import qualified Qi.Program.Config.Ipret.State         as Config
import           Qi.Program.Config.Lang as Config
import           Qi.Test.Logger
import           Test.Tasty.Hspec

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
                     Config.s3Kf kfName bucketId

          runConfig configProgram =
                snd
              . run
              . runState (mkConfig appName)
              $ Config.run configProgram

-- https://github.com/frontrowed/stratosphere/blob/master/library-gen/Stratosphere/Resources/S3Bucket.hs
--
      it "KF resource is rendered correctly" $ do
        let expectedKfLogicalId = kfName <> "KinesisFirehose"
            expectedKfPhysicalId = show appName <> "." <> kfName <> ".kinesis-firehose"

        case KF.toResources config of
          Resources [Resource kfLogicalId (ResourceProperties _ props) _ _ _ _ _ _
                    ] -> do

            kfLogicalId `shouldBe` expectedKfLogicalId

            let assertProp ps propKey expectedTextValue =
                  SHM.lookup propKey ps `shouldBe` Just (String expectedTextValue)


            let withProps ps cont =
                  let propShouldBe = assertProp ps
                      withSubProps propKey cont' =
                        case SHM.lookup propKey props of
                          Nothing -> panic $ "no top level property: " <> propKey
                          Just (Object subprops) -> cont' (assertProp subprops)
                          Just unexpected -> panic $ "unexpected value under key, expected Object but got: " <> show unexpected

                  in  cont (propShouldBe, withSubProps)

            withProps props $ \(propShouldBe, withSubProps) -> do
              "DeliveryStreamName" `propShouldBe` expectedKfPhysicalId
              "DeliveryStreamType" `propShouldBe` "DirectPut"

              withSubProps "S3DestinationConfiguration" $
                \(propShouldBe') -> do
                  "BucketARN" `propShouldBe'` "aws:s3::testapp.mybucket.s3-bucket"

            -- view sbBucketName kf `shouldBe` Just (Literal expectedKfPhysicalId)
      -- https://github.com/frontrowed/stratosphere/blob/master/library-gen/Stratosphere/ResourceProperties/S3BucketNotificationConfiguration.hs
            -- view sbNotificationConfiguration bucket `shouldBe` expectedNotificationConfig

          unexpected -> panic $ "unexpected number of resources created: " <> show unexpected
