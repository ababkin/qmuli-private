{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Qi.AWS.CF where

import           Control.Monad.Fail (fail)
import           Data.Aeson
import qualified Data.Text          as T
import           Protolude
import           Qi.AWS.ARN
import           Qi.AWS.Types


newtype LogicalResourceId = LogicalResourceId Text
  deriving (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)
