module Qi.Test.Config.Eff where

import           Control.Lens
import           Control.Monad.Freer           hiding (run)
import           Control.Monad.Freer.State
import           Data.Default                  (def)
import qualified Data.HashMap.Strict           as SHM
import           Protolude                     hiding (State, get, put,
                                                runState)
import           Qi.AWS.Resource
import           Qi.AWS.Types                  (AwsMode (LocalStack))
import           Qi.Config.AWS
import           Qi.Config.AWS.S3
import qualified Qi.Program.Config.Ipret.State as Config
import           Qi.Program.Config.Lang
import qualified Qi.Program.Wiring.IO          as IO
import           Qi.Test.Logger
import           Test.Tasty.Hspec


spec :: Spec
spec = parallel $
  describe "ConfigEff" $ do
    it "inserts an S3 bucket into the S3 config" $ do
      let exec = IO.run def LocalStack mkTestLogger $ do
            s3Bucket "mybucket" def
            getConfig

          expected = def & s3Config . s3IdToBucket .~ expectedS3Buckets
          expectedS3Buckets = SHM.singleton (mkS3BucketId "mybucket") (S3Bucket "mybucket" def [])

      exec `shouldReturn` expected


