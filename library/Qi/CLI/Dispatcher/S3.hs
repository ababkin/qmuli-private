{-# LANGUAGE ConstraintKinds     #-}

module Qi.CLI.Dispatcher.S3 where

import           Control.Lens
import           Polysemy
import           Protolude              hiding (all)

import           Qi.AWS.Resource
import           Qi.AWS.S3              (S3Bucket, S3Object)
import           Qi.AWS.Types
import           Qi.Config
import           Qi.Program.Config.Lang (getConfig, ConfigEff)
import           Qi.Program.Gen.Lang
import           Qi.Program.S3.Lang (ListToken, listObjects, deleteObjects, S3Eff)


type S3DispEffects effs = (Member ConfigEff effs, Member GenEff effs, Member S3Eff effs)

clearBuckets
  :: S3DispEffects effs
  => Sem effs ()
clearBuckets  = do
  config <- getConfig
  let bucketIds = map fst ( all config :: [ ( LogicalId 'S3BucketResource, S3Bucket ) ] )
  say "destroying buckets..."
  for_ bucketIds $ \bucketId -> do
    say $ "destroying bucket: '" <> show bucketId <> "'"
    forAll bucketId deleteObjects

  where
    forAll
      :: S3DispEffects effs
      => S3BucketId
      -> ([S3Object] -> Sem effs ())
      -> Sem effs ()
    forAll bucketId action = go bucketId action Nothing False

    go
      :: S3DispEffects effs
      => S3BucketId
      -> ([S3Object] -> Sem effs ())
      -> Maybe ListToken
      -> Bool
      -> Sem effs ()
    go _ _ Nothing True = pass
    go bucketId action maybeToken _ = do
      (objs, maybeToken') <- listObjects bucketId maybeToken
      action objs
      go bucketId action maybeToken' True
