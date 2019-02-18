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
import           Qi.AWS.Lambda             hiding (LambdaId)
import           Qi.AWS.S3
import           Qi.AWS.Types
import           Qi.Config
import           Qi.Program.Config.Lang


run
  :: forall effs a
  .  (Member (State Config) effs)
  => Eff (ConfigEff ': effs) a -> Eff effs a
run = interpret (\case

  GetConfig -> get

  RegGenericLambda inProxy outProxy name f profile ->
    withLogicalId name $ \lid -> do
      let lbd = GenericLambda profile inProxy outProxy f

      modify (lbdConfig . lbdIdToLambda %~ SHM.insert lid lbd)
      pure $ ConfigSuccess lid

-- S3
  RegS3Bucket name profile ->
    withLogicalId name $ \lid -> do
      let newBucket = def & s3bProfile .~ profile

      modify (s3Config . s3IdToBucket %~ SHM.insert lid newBucket)
      pure $ ConfigSuccess lid


  RegS3BucketLambda name bucketId f profile -> do
    withLogicalId name $ \lid -> do
      let lbd = S3BucketLambda profile f
          modifyBucket = s3bEventConfigs %~ ((S3EventConfig S3ObjectCreatedAll lid):)

      modify (s3Config . s3IdToBucket %~ SHM.adjust modifyBucket bucketId)
      modify (lbdConfig . lbdIdToLambda %~ SHM.insert lid lbd)

      pure $ ConfigSuccess lid
  )

  where
    withLogicalId name cont =
      case mkLogicalId name of
        Left err -> pure . ConfigFailure $ NameInvalid err
        Right lid -> do
          -- TODO: check if id is already used for a resource of this type
          cont lid
