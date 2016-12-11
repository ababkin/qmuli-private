{-# LANGUAGE OverloadedStrings #-}

module Config.Api where

import           Control.Lens
import           Control.Monad.State.Strict           (runState)
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Default                         (def)
import qualified Data.HashMap.Strict                  as SHM
import           Data.Maybe                           (fromJust, isJust)
import           Data.Text                            (Text)
import qualified Test.Tasty
import           Test.Tasty.Hspec

import           Qi.Config.AWS                        (Config (..))
import           Qi.Config.CF                         as CF
import           Qi.Program.Config.Interface          (ConfigProgram, api, apiResource)
import qualified Qi.Program.Config.Interpreters.Build as CB

spec :: Spec
spec = parallel $
  describe "Api" $ do
    it "contains Resources" $ do
      template `shouldContainKey` "Resources"

    it "contains Outputs" $ do
      template `shouldContainKey` "Outputs"

    it "outputs api url" $ do
      outputs `shouldContainKey` "worldApiURL"

  where
    outputs = fromJust $ template ^? key "Outputs"

    template = fromJust (decode $ CF.render config)

    configProgram :: ConfigProgram ()
    configProgram = do
      -- create a REST API
      api "world" >>= \world ->
        -- create a "things" resource
        apiResource "things" world >>= \things -> return ()
      return ()

    appName = "testName"
    config = snd . (`runState` def{_namePrefix = appName}) $ CB.interpret configProgram

shouldContainKey :: Value -> Text -> IO ()
shouldContainKey value tag = ( isJust $ value ^? key tag ) `shouldBe` True

