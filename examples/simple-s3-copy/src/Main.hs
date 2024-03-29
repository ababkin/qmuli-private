{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Aeson (Value (String))
import Data.Default (def)
import Polysemy
import Protolude
import Qi (withConfig)
import Qi.AWS.Lambda.Function
import Qi.AWS.S3 (s3eObject, s3oBucketId)
import Qi.AWS.Types (S3BucketId)
import Qi.Program.Config.Lang (s3Bucket, s3BucketLambda)
import Qi.Program.Gen.Lang (GenEff, say)
import Qi.Program.S3.Lang
  ( S3Eff,
    S3LambdaProgram,
    getContent,
    putContent,
  )

main :: IO ()
main = withConfig config
  where
    config = do
      -- create an "input" s3 bucket
      incoming <- s3Bucket "incoming" def

      -- create an "output" s3 bucket
      outgoing <- s3Bucket "outgoing" def

      -- create a lambda, which will copy an s3 object from "incoming" to "outgoing" buckets
      -- upon an S3 "Put" event.
      -- Attach the lambda to the "incoming" bucket such way so each time a file is uploaded to
      -- the bucket, the lambda is called with the information about the newly uploaded file.
      -- The lambda creation function takes the Lambda name, s3BucketId to attach to, lambda
      -- function itself and a lambda profile, that specifies attributes like memory size and
      -- timeout, and has meaningful defaults for those.
      void $
        s3BucketLambda "copyS3Object" incoming (copyContentsLambda outgoing) $
          def & lfpMemorySize .~ M1536

    copyContentsLambda ::
      (Member S3Eff effs, Member GenEff effs) =>
      S3BucketId ->
      S3LambdaProgram effs
    copyContentsLambda sinkBucketId = lbd
      where
        lbd event = do
          let incomingS3Obj = event ^. s3eObject
              outgoingS3Obj = s3oBucketId .~ sinkBucketId $ incomingS3Obj

          say "getting content"
          -- get the content of the newly uploaded file
          eitherContent <- getContent incomingS3Obj

          case eitherContent of
            Right content -> do
              say "putting content"
              -- write the content into a new file in the "output" bucket
              putContent outgoingS3Obj content

              pure $ String "lambda had executed successfully"
            Left err ->
              pure . String . toS $ "error: '" <> err <> "'"
