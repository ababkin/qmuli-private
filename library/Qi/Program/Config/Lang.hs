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
import           Qi.AWS.Lambda       (LambdaProfile, AllLambdaEffects)
import           Qi.AWS.S3           (S3BucketProfile)
import           Qi.AWS.KF
import           Qi.AWS.CW (CwLambdaProgram, CwEventsRuleProfile)
import           Qi.AWS.IAM
import           Qi.Program.Gen.Lang
import           Qi.Program.S3.Lang
import           Qi.Program.KF.Lang
import           Qi.Program.Lambda.Lang
import           Qi.AWS.Types


type S3BucketId = LogicalId 'S3BucketResource

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

