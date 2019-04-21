{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Data.Aeson
import           Data.Default           (def)
import           Protolude hiding (put)
import           Polysemy

import           Qi                     (withConfig)
import           Qi.AWS.Lambda
import           Qi.AWS.KF        (KfId)
import           Qi.AWS.S3              (s3eObject, s3oBucketId)
import           Qi.Program.Config.Lang (s3BucketKf, s3Bucket, cwEventLambda)
import           Qi.Program.Gen.Lang    (GenEff, say)
import           Qi.Program.KF.Lang    (KfEff, put)
import           Qi.AWS.CW 


main :: IO ()
main = withConfig config
  where
    config = do
      bucketId <- s3Bucket "kfdest" def
      kfId <- s3BucketKf "mykf" bucketId

      let ruleProfile = ScheduledEventProfile "cron(* * * * ? *)"
      void $ cwEventLambda "myEventLambda" ruleProfile (eventLambda kfId) def

    eventLambda
      :: (Member GenEff effs, Member KfEff effs)
      => KfId
      -> CwLambdaProgram effs
    eventLambda kfId _ = do
      -- emit log messages that end up in the appropriate cloudwatch group/stream
      put kfId $ object [ "da" .= Number 3 ]
      pure "lambda had executed successfully"
