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
  | Dynamo
  | CwEvents
  | Lambda
  | Sqs
  deriving (Eq, Show)

toNamespace :: Service -> Text
toNamespace S3 = "s3"
toNamespace Dynamo = "dynamodb"
toNamespace Lambda = "lambda"
toNamespace CwEvents = "events"
toNamespace Sqs = "sqs"

fromNamespace :: Text -> Maybe Service
fromNamespace "s3" = Just S3
fromNamespace "dynamodb" = Just Dynamo
fromNamespace "lambda" = Just Lambda
fromNamespace "events" = Just CwEvents
fromNamespace "sqs" = Just Sqs
fromNamespace _ = Nothing

toUrl :: Service -> Text
toUrl s = toNamespace s <> ".amazonaws.com"
