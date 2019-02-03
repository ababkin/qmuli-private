{-# LANGUAGE DeriveAnyClass #-}

module Qi.Config.Identifier where

import           Data.Aeson    (FromJSON, ToJSON)
import           Data.Hashable
import           Protolude


class FromInt a where
  fromInt :: Int -> a

newtype S3BucketId = S3BucketId Int deriving (Eq, Show, Hashable, Generic, ToJSON, FromJSON)
instance FromInt S3BucketId where
  fromInt = S3BucketId

newtype LambdaId = LambdaId Int deriving (Eq, Show, Hashable, Generic, ToJSON, FromJSON)
instance FromInt LambdaId where
  fromInt = LambdaId

newtype KfId = KfId Int deriving (Eq, Show, Hashable, Generic, ToJSON, FromJSON)
instance FromInt KfId where
  fromInt = KfId
