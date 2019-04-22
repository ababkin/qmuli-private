{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Qi.Program.Config.Ipret.State where

import           Control.Lens              hiding (view)
import           Data.Default              (def)
import qualified Data.HashMap.Strict       as SHM
import           Data.Proxy                (Proxy (Proxy))
import           Protolude                 hiding (State, get, gets, modify,
                                            runState)
import           Polysemy
import           Polysemy.State

import           Qi.AWS.Lambda 
import           Qi.AWS.S3
import           Qi.AWS.KF
import           Qi.AWS.CW
import           Qi.AWS.IAM
import           Qi.AWS.Types
import           Qi.Config
import qualified Qi.Program.Config.Lang as Lang
import           Qi.AWS.ARN


run
  :: forall effs a
  .  (Member (State Config) effs)
  => Sem (Lang.ConfigEff ': effs) a -> Sem effs a
run = interpret (\case

  Lang.GetConfig -> get

  Lang.GenericLambda inProxy outProxy name f profile ->
    withLogicalId name $ \lid -> do
      roleId <- insertRole name lid
      let lbd = GenericLambda roleId profile inProxy outProxy f
      modify (lbdConfig . lbdIdToLambda %~ SHM.insert lid lbd)
      pure lid

-- S3
  Lang.S3Bucket name profile ->
    withLogicalId name $ \lid -> do
      let newBucket = def & s3bProfile .~ profile
      modify (s3Config . s3IdToBucket %~ SHM.insert lid newBucket)
      pure lid

  Lang.S3BucketLambda name bucketId f profile -> do
    withLogicalId name $ \lid -> do
      roleId <- insertRole name lid
      let lbd = S3BucketLambda roleId profile f
          modifyBucket = s3bEventConfigs %~ ((S3EventConfig S3ObjectCreatedAll lid):)
      modify (s3Config . s3IdToBucket %~ SHM.adjust modifyBucket bucketId)
      modify (lbdConfig . lbdIdToLambda %~ SHM.insert lid lbd)
      pure lid

  Lang.CwEventLambda name ruleProfile programFunc profile -> do
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

  Lang.S3BucketKf name bucketId -> do
    withLogicalId name $ \lid -> do
      roleId <- insertRole name lid
      let kf = Kf def roleId bucketId
      modify (kfConfig . kfIdToKf %~ SHM.insert lid kf)
      pure lid

  )

  where
    insertRole
      :: forall principalId
      .  (ToArn principalId)
      => Text
      -> principalId
      -> Sem effs (LogicalId 'IamRoleResource)
    insertRole name lid =
      withLogicalId name $ \roleId -> do
        config <- get
        let role = IamRole $ toArn lid $ config ^. appName
        modify (iamConfig . idToRole %~ SHM.insert roleId role)
        pure roleId

    withLogicalId name cont =
      case mkLogicalId name of
        Left err -> panic $ "invalid name used for logical name: " <> show err
        Right lid -> cont lid
