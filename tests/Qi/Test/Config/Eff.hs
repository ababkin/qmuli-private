{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Qi.Test.Config.Eff where

import Control.Lens
import Data.Default (def)
import qualified Data.HashMap.Strict as SHM
import Polysemy
import Protolude
import Qi.AWS.S3
import Qi.AWS.Types
import Qi.Config
import qualified Qi.Program.Config.Ipret.State as Config
import Qi.Program.Config.Lang (getConfig, s3Bucket)
import Qi.Program.S3.Lang (getContent)
import qualified Qi.Program.Wiring.IO as IO
import Qi.Test.Logger
import Test.Tasty.Hspec

spec :: Spec
spec = parallel $
  describe "ConfigEff" $ do
    it "inserts an S3 bucket into the S3 config" $ do
      let bucketName = "mybucket"
          Right initialConfig = mkConfig <$> mkAppName "testApp"
          exec = IO.run initialConfig LocalStack mkTestLogger $ do
            bucketId <- s3Bucket bucketName def
            -- getContent $ S3Object bucketId (S3Key "someKey")
            getConfig

          expected = initialConfig & s3Config . idToBucket .~ expectedS3Buckets
          Right lid = mkLogicalId bucketName
          expectedS3Buckets = SHM.singleton lid (S3Bucket def [])

      exec `shouldReturn` expected
