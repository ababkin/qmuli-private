{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Program.Config.Interpreters.Build where

import           Control.Lens                hiding (view)
import           Control.Monad.Operational
import           Control.Monad.State.Strict  (State, get, put)
import           Data.Default                (def)
import           Data.Hashable               (hash)
import qualified Data.HashMap.Strict         as SHM
import           Data.Monoid                 ((<>))

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier
import           Qi.Program.Config.Interface (ConfigInstruction (CreateS3Bucket, CreateS3BucketLambda),
                                              ConfigProgram)


interpret
  :: ConfigProgram ()
  -> State Config ()
interpret program =  do
  case view program of
    (CreateS3Bucket name) :>>= is -> do
      interpret . is =<< createS3Bucket name

    (CreateS3BucketLambda name bucketId lbdProgramFunc) :>>= is -> do
      interpret . is =<< createS3BucketLambda name bucketId lbdProgramFunc

    Return _ ->
      return def


  where

    createS3Bucket name = do
      conf <- get

      let newBucket = S3Bucket {
            _s3bName = name
          , _s3bLbdEventConfigs = []
          }

      let (newBucketId, s3ConfigModifier) = insertBucket newBucket

      put $ over s3Config s3ConfigModifier conf

      return newBucketId


    createS3BucketLambda name bucketId lbdProgramFunc = do
      conf <- get

      let newLambda = S3BucketLambda name bucketId lbdProgramFunc
          newLambdaIdentifier = LambdaIdentifier $ hash newLambda

          modifyBucket = over s3bLbdEventConfigs ((LambdaEventConfig S3ObjectCreatedAll newLambdaIdentifier):)
          modifiedConf = over (s3Config . s3Buckets . s3idxIdToS3Bucket) (SHM.adjust modifyBucket bucketId) conf

      put $ modifiedConf <> def{_lbdConfig = def{_lcLambdas = SHM.singleton newLambdaIdentifier newLambda}}

      return newLambdaIdentifier
