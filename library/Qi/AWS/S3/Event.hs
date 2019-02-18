{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.AWS.S3.Event where

import           Control.Lens
import           Control.Monad.Fail (fail)
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hashable
import qualified Data.Text          as T
import           Protolude
import           Qi.AWS.S3
import           Qi.AWS.Types
import           Qi.Config


parse
  :: Value
  -> Parser S3Event
parse = withObject "S3Event" $ \o -> do
  firstRecord <- headMay <$> o .: "Records"
  -- TODO: should we consider cases where there are more than one records? (probably yes)
  case firstRecord of
    Nothing ->
      fail "no records"
    Just record -> do
      s3        <- record .: "s3"
      bucketId  <- (.: "name") =<< s3 .: "bucket"
      key       <- (.: "key")  =<< s3 .: "object"
      case parseS3BucketPhysicalId bucketId of
        Left err -> fail $ "could not parse s3 bucket physical id: " <> show bucketId <>
                    ", error was: " <> show err
        Right pid ->
          pure . S3Event $ S3Object (toLogicalId pid) (S3Key key)
