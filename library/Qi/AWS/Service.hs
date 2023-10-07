module Qi.AWS.Service
  ( Service (..),
    toNamespace,
    fromNamespace,
    toUrl,
  )
where

import Protolude

data Service
  = S3
  | Lambda
  deriving (Eq, Show)

toNamespace :: Service -> Text
toNamespace S3 = "s3"
toNamespace Lambda = "lambda"

fromNamespace :: Text -> Maybe Service
fromNamespace "s3" = Just S3
fromNamespace "lambda" = Just Lambda
fromNamespace _ = Nothing

toUrl :: Service -> Text
toUrl s = toNamespace s <> ".amazonaws.com"
