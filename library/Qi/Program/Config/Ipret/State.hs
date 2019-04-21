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
import           Qi.AWS.KF
import           Qi.AWS.CW
import           Qi.AWS.IAM
import           Qi.AWS.Types
import           Qi.Config
import           Qi.Program.Config.Lang
import           Qi.AWS.ARN


run
  :: forall effs a
  .  (Member (State Config) effs)
  => Eff (ConfigEff ': effs) a -> Eff effs a
run = interpret (\case

  GetConfig -> get

  RegGenericLambda inProxy outProxy name f profile ->
    withLogicalId name $ \lid -> do
      roleId <- insertRole name lid
      let lbd = GenericLambda roleId profile inProxy outProxy f
      modify (lbdConfig . lbdIdToLambda %~ SHM.insert lid lbd)
      pure lid

-- S3
  RegS3Bucket name profile ->
    withLogicalId name $ \lid -> do
      let newBucket = def & s3bProfile .~ profile
      modify (s3Config . s3IdToBucket %~ SHM.insert lid newBucket)
      pure lid

  RegS3BucketLambda name bucketId f profile -> do
    withLogicalId name $ \lid -> do
      roleId <- insertRole name lid
      let lbd = S3BucketLambda roleId profile f
          modifyBucket = s3bEventConfigs %~ ((S3EventConfig S3ObjectCreatedAll lid):)
      modify (s3Config . s3IdToBucket %~ SHM.adjust modifyBucket bucketId)
      modify (lbdConfig . lbdIdToLambda %~ SHM.insert lid lbd)
      pure lid

  RegCwEventLambda name ruleProfile programFunc profile -> do
    withLogicalId name $ \lid -> do
      roleId <- insertRole name lid
      let lbd = CwEventLambda roleId profile programFunc
          eventsRule = CwEventsRule {
            _cerName    = name
          , _cerProfile = ruleProfile
          , _cerLbdId   = lid
          }
      withLogicalId (name <> "EventsRule") $ \eventsRuleId -> do
        modify $ cwConfig . ccRules %~ SHM.insert eventsRuleId eventsRule
        modify (lbdConfig . lbdIdToLambda %~ SHM.insert lid lbd)
      pure lid

  RegS3BucketKf name bucketId -> do
    withLogicalId name $ \lid -> do
      roleId <- insertRole name lid
      let kf = Kf def roleId bucketId
      modify (kfConfig . kfIdToKf %~ SHM.insert lid kf)
      pure lid


  -- RegIamRole name -> do
  --   withLogicalId name $ \lid -> do
  --     let role = IamRole
  --     modify (iamConfig . idToRole %~ SHM.insert lid role)
  --     pure lid

  )

  where
    -- insertRole
    --   :: forall (principal :: AwsResourceType)
    --   .  ToArn principal
    --   => Text
    --   -> LogicalId principal
    --   -> Eff effs (LogicalId 'IamRoleResource)
    insertRole name lid =
      withLogicalId name $ \roleId -> do
        -- config <- appName %. get
        config <- get
        let role = IamRole $ toArn lid $ config ^. appName
        modify (iamConfig . idToRole %~ SHM.insert roleId role)
        pure roleId

    -- withLogicalId :: Text -> (forall (principal :: AwsResourceType) . LogicalId principal -> b) -> b
    withLogicalId name cont =
      case mkLogicalId name of
        Left err -> panic $ "invalid name used for logical name: " <> show err
        Right lid -> cont lid
