{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Qi.Amazonka where

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Either (either)
import Network.AWS hiding (send)
import Protolude
import Text.Heredoc

qmuliConfig :: LBS.ByteString
qmuliConfig = [there|./qmuli.json|]

newtype CurrentRegion = CurrentRegion {unCurrentRegion :: Region}

instance FromJSON CurrentRegion where
  parseJSON = withObject "CurrentRegion" $ \o ->
    CurrentRegion <$> (withObject "AWS configuration" (.: "region") =<< o .: "aws")

currentRegion :: Region
currentRegion =
  unCurrentRegion $
    either
      (panic . ("could not parse qmuli.yaml: " <>) . toS)
      identity
      $ eitherDecode qmuliConfig
