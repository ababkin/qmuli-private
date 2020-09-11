module Main where

import Protolude
{- import           Qi.Config.AWS.CF -}
-- import qualified Qi.Test.CLI.Deploy               as Deploy
import qualified Qi.Test.Config.Eff as ConfigEff
import qualified Qi.Test.Config.Render as Render
import qualified Qi.Test.Config.Render.KF as Render.KF
import qualified Qi.Test.Config.Render.S3 as Render.S3
import qualified Qi.Test.Resource as Resource
-- import           Qi.Test.Integration.SimpleS3Copy as SimpleS3Copy
{- import           Test.Tasty -}
{- import           Test.Tasty.Hspec -}
import Test.Hspec

main :: IO ()
main = hspec $ do
  -- SimpleS3Copy.spec
  ConfigEff.spec
  Render.spec
  Render.S3.spec
  Render.KF.spec
  Resource.spec

-- Deploy.spec
