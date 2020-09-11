-- |
module Qi.Test.Util where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (key)
import qualified Data.HashMap.Strict as SHM
import Data.Maybe (fromJust)
import Protolude
import Test.Tasty.Hspec

-- shouldContainKey
--   :: Value
--   -> Text
--   -> IO ()
-- shouldContainKey template tag = isJust (template ^? key tag) `shouldBe` True

-- shouldContainKVPair
--   :: Value
--   -> (Text, Value)
--   -> IO ()
-- shouldContainKVPair template (tag, val) = fromJust (template ^? key tag) `shouldBe` val

-- getValueUnderKey
--   :: Text
--   -> Value
--   -> Value
-- getValueUnderKey k t = fromJust $ t ^? key k

-- ref :: Text -> Value
-- ref lname = object [("Ref", String lname)]

assertProp ::
  (Show v, Eq v, Eq k, Hashable k) =>
  SHM.HashMap k v ->
  k ->
  v ->
  Expectation
assertProp ps propKey expectedValue =
  SHM.lookup propKey ps `shouldBe` Just expectedValue

withProps ::
  SHM.HashMap Text Value ->
  ( ( Text -> Value -> Expectation,
      Text -> Object
    ) ->
    t
  ) ->
  t
withProps ps cont =
  let propShouldBe = assertProp ps
      subProps propKey =
        case SHM.lookup propKey ps of
          Nothing -> panic $ "no top level property: " <> propKey
          Just (Object subprops) -> subprops -- withProps subprops cont' -- cont' (assertProp subprops)
          Just unexpected -> panic $ "unexpected value under key, expected Object but got: " <> show unexpected
   in cont (propShouldBe, subProps)
