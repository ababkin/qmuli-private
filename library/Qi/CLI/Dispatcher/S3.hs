module Qi.CLI.Dispatcher.S3 where

import           Control.Lens
import           Control.Monad.Freer    hiding (send)
import           Protolude              hiding (all)
import           Qi.AWS.Resource
import           Qi.AWS.S3              (S3Bucket)
import           Qi.AWS.Types
import           Qi.Config
import           Qi.Program.Config.Lang
import           Qi.Program.Gen.Lang
import           Qi.Program.S3.Lang


clearBuckets
  :: Members '[ S3Eff, GenEff, ConfigEff ] effs
  => Eff effs ()
clearBuckets  = do
  config <- getConfig
  let bucketIds = map fst ( all config :: [ ( LogicalId 'S3BucketResource, S3Bucket ) ] )
  say "destroying buckets..."
  for_ bucketIds $ \bucketId -> do
    say $ "destroying bucket: '" <> show bucketId <> "'"
    forAll bucketId deleteObjects

  where
    forAll bucketId action = go bucketId action Nothing False

    go _ _ Nothing True = pass
    go bucketId action maybeToken _ = do
      (objs, maybeToken') <- listObjects bucketId maybeToken
      action objs
      go bucketId action maybeToken' True


