{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Qi.Program.Config.Lang where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Default (Default, def)
import Polysemy
import Protolude
import Qi.AWS.CW
import Qi.AWS.IAM
import Qi.AWS.KF
import Qi.AWS.Lambda.EventSourceMapping
import Qi.AWS.Lambda.Function
import Qi.AWS.S3 (S3BucketProfile, S3Event)
import Qi.AWS.SQS
import Qi.AWS.Types
import Qi.Config (Config)
import Qi.Program.Gen.Lang
import Qi.Program.KF.Lang
import Qi.Program.Lambda.Lang
import Qi.Program.S3.Lang

data ConfigEff m r where
  GetConfig ::
    ConfigEff m Config
  S3Bucket ::
    Text ->
    S3BucketProfile ->
    ConfigEff m S3BucketId
  KfStreamS3 ::
    Text ->
    S3BucketId ->
    ConfigEff m KfStreamId
  GenericLambda ::
    (FromJSON a, ToJSON b) =>
    Proxy a ->
    Proxy b ->
    Text ->
    (forall effs. AllLambdaEffects effs => a -> Sem effs b) ->
    LambdaFunctionProfile ->
    ConfigEff m LambdaFunctionId
  S3BucketLambda ::
    Text ->
    S3BucketId ->
    (forall effs. AllLambdaEffects effs => S3Event -> Sem effs Value) ->
    LambdaFunctionProfile ->
    ConfigEff m LambdaFunctionId
  CwEventLambda ::
    Text ->
    CwEventsRuleProfile ->
    (forall effs. AllLambdaEffects effs => CwEvent -> Sem effs Value) ->
    LambdaFunctionProfile ->
    ConfigEff m LambdaFunctionId
  SqsLambda ::
    Text ->
    QueueId ->
    LambdaEventSourceMappingProfile ->
    (forall effs. AllLambdaEffects effs => SqsEvent -> Sem effs Value) ->
    LambdaFunctionProfile ->
    ConfigEff m LambdaFunctionId

makeSem ''ConfigEff
