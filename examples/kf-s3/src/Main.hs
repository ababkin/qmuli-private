{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Data.Aeson
import           Data.Default           (def)
import           Protolude hiding (put)
import           Polysemy

import           Qi                     (withConfig)
import           Qi.AWS.Lambda.Function
import           Qi.AWS.Types        (KfStreamId)
import           Qi.AWS.S3              (s3eObject, s3oBucketId)
import           Qi.Program.Config.Lang (kfStreamS3, s3Bucket, cwEventLambda)
import           Qi.Program.Gen.Lang    (GenEff, say)
import           Qi.Program.KF.Lang    (KfEff, put)
import           Qi.AWS.CW 


main :: IO ()
main = withConfig config
  where
    config = do
      bucketId <- s3Bucket "kfdest" def
      kfStreamId <- kfStreamS3 "mykf" bucketId

      let ruleProfile = ScheduledEventProfile "cron(* * * * ? *)"
      void $ cwEventLambda "myEventLambda" ruleProfile (eventLambda kfStreamId) def

    eventLambda
      :: (Member GenEff effs, Member KfEff effs)
      => KfStreamId
      -> CwEvent -> Sem effs Value
    eventLambda kfStreamId _ = do
      -- emit log messages that end up in the appropriate cloudwatch group/stream
      put kfStreamId $ object [ "da" .= Number 3 ]
      pure $ String "lambda had executed successfully"
