{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Qi.Test.Config.Eff where

import           Control.Lens
import           Data.Default                  (def)
import qualified Data.HashMap.Strict           as SHM
import           Protolude
import Polysemy

import           Qi.AWS.Resource
import           Qi.AWS.S3
import           Qi.AWS.Types
import           Qi.Config
import qualified Qi.Program.Config.Ipret.State as Config
import           Qi.Program.Config.Lang (s3Bucket, getConfig)
import qualified Qi.Program.Wiring.IO          as IO
import           Qi.Test.Logger
import           Test.Tasty.Hspec


spec :: Spec
spec = parallel $
  describe "ConfigEff" $ do
    it "inserts an S3 bucket into the S3 config" $ do
      let bucketName = "mybucket"
          Right initialConfig = mkConfig <$> mkAppName "testApp"
          exec = IO.run initialConfig LocalStack mkTestLogger $ do
            s3Bucket bucketName def
            getConfig

          expected = initialConfig & s3Config . s3IdToBucket .~ expectedS3Buckets
          Right lid = mkLogicalId bucketName
          expectedS3Buckets = SHM.singleton lid (S3Bucket def [])

      exec `shouldReturn` expected


