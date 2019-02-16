{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}

module Qi.Program.Config.Ipret.State where

import           Control.Lens              hiding (view)
import           Control.Monad.Freer
import           Control.Monad.Freer.State
import           Data.Default              (def)
import qualified Data.HashMap.Strict       as SHM
import           Data.Proxy                (Proxy (Proxy))
import           Protolude                 hiding (State, get, gets, modify,
                                            runState)
import           Qi.AWS.Resource           hiding (name)
import           Qi.AWS.Types              (LogicalId (..))
import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda      hiding (LambdaId)
import           Qi.Config.AWS.S3
import           Qi.Program.Config.Lang    (ConfigEff (..))


run
  :: forall effs a
  .  (Member (State Config) effs)
  => Eff (ConfigEff ': effs) a -> Eff effs a
run = interpret (\case

  GetConfig -> get

  RegGenericLambda inProxy outProxy name f profile -> do
      let lbd = GenericLambda name profile inProxy outProxy f
          lbdLogicalId = LogicalId name
          insertIdToLambda  = lbdIdToLambda %~ SHM.insert lbdLogicalId lbd

      modify (lbdConfig %~ insertIdToLambda)
      pure lbdLogicalId

-- S3
  RegS3Bucket name profile -> do
      let newBucket = def & s3bName .~ name
                          & s3bProfile .~ profile
          bucketLogicalId = LogicalId name
          insertIdToBucket = s3IdToBucket %~ SHM.insert bucketLogicalId newBucket

      modify (s3Config %~ insertIdToBucket)
      pure bucketLogicalId


  RegS3BucketLambda name bucketId f profile -> do
      let lbd = S3BucketLambda name profile f
          lbdLogicalId = LogicalId name
          modifyBucket = s3bEventConfigs %~ ((S3EventConfig S3ObjectCreatedAll lbdLogicalId):)
          insertIdToLambda  = lbdIdToLambda %~ SHM.insert lbdLogicalId lbd

      modify (s3Config . s3IdToBucket %~ SHM.adjust modifyBucket bucketId)
      modify (lbdConfig %~ insertIdToLambda)

      pure lbdLogicalId
  )
