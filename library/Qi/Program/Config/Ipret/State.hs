{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Qi.Program.Config.Ipret.State where

import Control.Lens hiding (mapping, view)
import Data.Aeson (Value)
import Data.Default (def)
import qualified Data.HashMap.Strict as SHM
import Data.Proxy (Proxy (Proxy))
import Polysemy
import Polysemy.State
import Protolude hiding
  ( State,
    get,
    gets,
    modify,
    runState,
  )
import Qi.AWS.ARN
import Qi.AWS.IAM
import Qi.AWS.Lambda
import Qi.AWS.Lambda.EventSourceMapping as ESM
import Qi.AWS.Lambda.Function
import Qi.AWS.Lambda.Permission
import Qi.AWS.S3
import Qi.AWS.Service
import Qi.AWS.Types
import Qi.Config hiding (mapping)
import qualified Qi.Program.Config.Lang as Lang

run ::
  forall effs a.
  (Member (State Config) effs) =>
  Sem (Lang.ConfigEff ': effs) a ->
  Sem effs a
run =
  interpret
    ( \case
        Lang.GetConfig -> get
        Lang.GenericLambda inProxy outProxy name program profile ->
          withLogicalId name $ \lid -> do
            insertPermission name lid Lambda
            roleId <- insertRole name lid
            let lbd = LambdaFunction roleId profile inProxy outProxy program
            modify (lbdConfig . idToFunction %~ SHM.insert lid lbd)
            pure lid

        -- S3
        Lang.S3Bucket name profile ->
          withLogicalId name $ \lid -> do
            let newBucket = def & s3bProfile .~ profile
            modify (s3Config . idToBucket %~ SHM.insert lid newBucket)
            pure lid
        Lang.S3BucketLambda name bucketId program profile -> do
          withLogicalId name $ \lid -> do
            insertPermission name lid S3
            roleId <- insertRole name lid
            let lbd = LambdaFunction roleId profile (Proxy :: Proxy S3Event) (Proxy :: Proxy Value) program
                modifyBucket = s3bEventConfigs %~ ((S3EventConfig S3ObjectCreatedAll lid) :)
            modify (s3Config . idToBucket %~ SHM.adjust modifyBucket bucketId)
            modify (lbdConfig . idToFunction %~ SHM.insert lid lbd)
            pure lid
    )
  where
    insertPermission ::
      Text ->
      LambdaId ->
      Service ->
      Sem effs LambdaPermissionId
    insertPermission name lid service =
      withLogicalId name $ \permissionId -> do
        let permission = LambdaPermission service lid
        modify (lbdConfig . idToPermission %~ SHM.insert permissionId permission)
        pure permissionId

    insertRole ::
      forall principalId.
      (ToArn principalId) =>
      Text ->
      principalId ->
      Sem effs RoleId
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
