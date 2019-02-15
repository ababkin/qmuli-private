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
import           Qi.AWS.Resource
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
      -- let lbd = GenericLambda name profile inProxy outProxy f
      insertLambda id name lbd

-- S3
  RegS3Bucket name profile -> do
      let newBucket = def & s3bName .~ name
                          & s3bProfile .~ profile
          insertIdToBucket = s3IdToBucket %~ SHM.insert id newBucket
          insertNameToId = s3NameToId %~ SHM.insert name id

      modify (s3Config %~ insertNameToId . insertIdToBucket)


  RegS3BucketLambda name bucketId f profile -> do
      let lbd = S3BucketLambda name profile f
          modifyBucket = s3bEventConfigs %~ ((S3EventConfig S3ObjectCreatedAll id):)
      modify (s3Config . s3IdToBucket %~ SHM.adjust modifyBucket bucketId)

      insertLambda id name lbd
  )

  where

    insertLambda
      :: LambdaId
      -> Text
      -> Lambda
      -> Eff effs ()
    insertLambda id name lbd = do

      let insertIdToLambda  = lbdIdToLambda %~ SHM.insert id lbd

      void $ modify (lbdConfig %~ insertNameToId . insertIdToLambda)
