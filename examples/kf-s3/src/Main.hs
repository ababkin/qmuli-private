{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson
import Data.Default (def)
import Polysemy
import Protolude hiding (put)
import Qi (withConfig)
import Qi.AWS.CW
import Qi.AWS.Lambda.Function
import Qi.AWS.S3 (s3eObject, s3oBucketId)
import Qi.AWS.Types (KfStreamId)
import Qi.Program.Config.Lang (cwEventLambda, kfStreamS3, s3Bucket)
import Qi.Program.Gen.Lang (GenEff, say)
import Qi.Program.KF.Lang (KfEff, put)

main :: IO ()
main = withConfig config
  where
    config = do
      bucketId <- s3Bucket "kfdest" def
      kfStreamId <- kfStreamS3 "mykf" bucketId

      let ruleProfile = ScheduledEventProfile "cron(* * * * ? *)"
      void $ cwEventLambda "myEventLambda" ruleProfile (eventLambda kfStreamId) def

    eventLambda ::
      (Member GenEff effs, Member KfEff effs) =>
      KfStreamId ->
      CwEvent ->
      Sem effs Value
    eventLambda kfStreamId _ = do
      -- emit log messages that end up in the appropriate cloudwatch group/stream
      put kfStreamId $ object ["da" .= Number 3]
      pure $ String "lambda had executed successfully"
