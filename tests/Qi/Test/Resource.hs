{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Qi.Test.Resource where

import Control.Lens
import Protolude hiding (State, get, put, runState)
import Qi.AWS.Types
import Qi.Config hiding (appName)
import Test.Tasty.Hspec

spec :: Spec
spec = parallel $
  describe "Resource" $ do
    it "parse physical id" $ do
      let Right appName = mkAppName "testApp"
      let Right (pid :: PhysicalId 'LambdaFunctionResource) = mkPhysicalId appName "mybucket"
      parsePhysicalId "testApp_mybucket_lambda-function" `shouldBe` Right pid
