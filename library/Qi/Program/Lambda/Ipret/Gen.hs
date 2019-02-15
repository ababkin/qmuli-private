{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}


module Qi.Program.Lambda.Ipret.Gen  where

import           Control.Lens           (Getting, (.~), (?~), (^.))
import           Control.Monad.Freer
import           Data.Aeson             (encode)
import           Network.AWS.Lambda     (InvocationType (Event),
                                         iInvocationType, invoke, lambda,
                                         uS3Bucket, uS3Key, updateFunctionCode)
import           Network.AWS.S3         (ObjectKey (ObjectKey))
import           Protolude              hiding ((<&>))
import           Qi.AWS.Resource
import           Qi.AWS.Types
import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Program.Config.Lang (ConfigEff, getConfig)
import           Qi.Program.Gen.Lang
import           Qi.Program.Lambda.Lang (LambdaEff (..))


type LambdaId = LogicalId 'LambdaResource

run
  :: forall effs a
  .  (Member GenEff effs, Member ConfigEff effs)
  => (Eff (LambdaEff ': effs) a -> Eff effs a)
run = interpret (\case

  Invoke id payload -> do
    config  <- getConfig
    let pid = getLambdaPhysicalId config id
    void . amazonka lambda $ invoke (unPhysicalId pid) (toS $ encode payload)
                        & iInvocationType ?~ Event


  Update id S3Object{ _s3oBucketId, _s3oKey = S3Key s3Key } -> do
    config  <- getConfig
    let pid = getLambdaPhysicalId config id
        bucketPid = physicalId config (getById config _s3oBucketId :: S3Bucket)
    void . amazonka lambda $ updateFunctionCode (unPhysicalId pid)
                        & uS3Bucket ?~ unPhysicalId bucketPid
                        & uS3Key    ?~ s3Key


  )

  where
    -- getLambdaPhysicalId :: Config -> LogicalId (ResourceType Lambda) -> PhysicalId (ResourceType Lambda)
    getLambdaPhysicalId config lid =
        let
          lambda :: Lambda = getById config lid
        in
          physicalId config lambda
