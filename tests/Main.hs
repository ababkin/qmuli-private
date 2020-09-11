module Main where

import Protolude
import qualified Qi.Test.Config.Eff as ConfigEff
import qualified Qi.Test.Config.Render as Render
import qualified Qi.Test.Config.Render.S3 as Render.S3
import qualified Qi.Test.Resource as Resource
import Test.Hspec

main :: IO ()
main = hspec $ do
  ConfigEff.spec
  Render.spec
  Render.S3.spec
  Resource.spec
