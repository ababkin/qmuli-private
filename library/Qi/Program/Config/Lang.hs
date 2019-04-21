{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ConstraintKinds     #-}

module Qi.Program.Config.Lang where

import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Default               (Default, def)
import           Protolude
import           Polysemy

import           Qi.Config              (Config)
import           Qi.AWS.Lambda       (LambdaProfile)
import           Qi.AWS.S3           (S3BucketProfile)
import           Qi.AWS.KF
import           Qi.AWS.CW
import           Qi.AWS.IAM
import           Qi.Program.Gen.Lang
import           Qi.Program.S3.Lang
import           Qi.Program.KF.Lang
import           Qi.AWS.Types


type S3BucketId = LogicalId 'S3BucketResource
type AllLambdaEffects effs = (Member GenEff effs, Member KfEff effs, Member S3Eff effs)

data ConfigEff m r where

  GetConfig
    :: ConfigEff m Config

  GenericLambda
    :: (FromJSON a, ToJSON b)
    => Proxy a
    -> Proxy b
    -> Text
    -> (forall effs . AllLambdaEffects effs => a -> Sem effs b)
    -> LambdaProfile
    -> ConfigEff m LambdaId

-- S3
  S3Bucket
    :: Text
    -> S3BucketProfile
    -> ConfigEff m S3BucketId

  S3BucketLambda
    :: Text
    -> S3BucketId
    -> (forall effs . AllLambdaEffects effs => S3LambdaProgram effs)
    -> LambdaProfile
    -> ConfigEff m LambdaId

  CwEventLambda
    :: Text
    -> CwEventsRuleProfile
    -> (forall effs . AllLambdaEffects effs => CwLambdaProgram effs)
    -> LambdaProfile
    -> ConfigEff m LambdaId

  S3BucketKf
    :: Text
    -> S3BucketId
    -> ConfigEff m KfId

makeSem ''ConfigEff

-- getConfig
--   :: forall effs
--   .  (Member ConfigEff effs)
--   => Eff effs Config
-- getConfig = send GetConfig

-- genericLambda
--   :: forall a b resEffs
--   .  (Member ConfigEff resEffs, FromJSON a, ToJSON b)
--   => Text
--   -> (forall effs . (Member GenEff effs, Member S3Eff effs) => a -> Eff effs b)
--   -> LambdaProfile
--   -> Eff resEffs LambdaId
-- genericLambda name f =
--   send . RegGenericLambda (Proxy :: Proxy a) (Proxy :: Proxy b) name f

-- s3Bucket
--   :: (Member ConfigEff effs)
--   => Text
--   -> S3BucketProfile
--   -> Eff effs S3BucketId
-- s3Bucket =
--   send .: RegS3Bucket

-- s3BucketLambda
--   :: forall resEffs
--   .  (Member ConfigEff resEffs)
--   => Text
--   -> S3BucketId
--   -> (forall effs . (Member GenEff effs, Member S3Eff effs) => S3LambdaProgram effs)
--   -> LambdaProfile
--   -> Eff resEffs LambdaId
-- s3BucketLambda name bucketId f =
--   send . RegS3BucketLambda name bucketId f

-- cwEventLambda
--   :: forall resEffs
--   .  (Member ConfigEff resEffs)
--   => Text
--   -> CwEventsRuleProfile
--   -> (forall effs . (Members [GenEff, KfEff] effs) => CwLambdaProgram effs)
--   -> LambdaProfile
--   -> Eff resEffs LambdaId
-- cwEventLambda name ruleProfile f =
--   send . RegCwEventLambda name ruleProfile f

-- s3Kf
--   :: forall resEffs
--   .  (Member ConfigEff resEffs)
--   => Text
--   -> S3BucketId
--   -> Eff resEffs KfId
-- s3Kf =
--   send .: RegS3BucketKf
