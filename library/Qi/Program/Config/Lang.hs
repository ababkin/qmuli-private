{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeOperators       #-}

module Qi.Program.Config.Lang where

import           Control.Monad.Freer
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Default               (Default, def)
import           Protolude
import           Qi.Config              (Config)
import           Qi.AWS.Lambda       (LambdaProfile)
import           Qi.AWS.S3           (S3BucketProfile)
import           Qi.AWS.KF
import           Qi.Core.Curry
import           Qi.Program.Gen.Lang
import           Qi.Program.S3.Lang
import           Qi.AWS.Types


type LambdaId = LogicalId 'LambdaResource
type S3BucketId = LogicalId 'S3BucketResource

-- data ConfigError = NameAlreadyUsed
--                  | NameInvalid Text

-- data ConfigResult a = ConfigFailure ConfigError
--                     | ConfigSuccess a

data ConfigEff r where

  GetConfig
    :: ConfigEff Config

  RegGenericLambda
    :: forall a b
    .  (FromJSON a, ToJSON b)
    => Proxy a
    -> Proxy b
    -> Text
    -> (forall effs . (Member GenEff effs, Member S3Eff effs) => a -> Eff effs b)
    -> LambdaProfile
    -> ConfigEff LambdaId

-- S3
  RegS3Bucket
    :: Text
    -> S3BucketProfile
    -> ConfigEff S3BucketId

  RegS3BucketLambda
    :: Text
    -> S3BucketId
    -> (forall effs . (Member GenEff effs, Member S3Eff effs) => S3LambdaProgram effs)
    -> LambdaProfile
    -> ConfigEff LambdaId

  RegS3BucketKf
    :: Text
    -> S3BucketId
    -> ConfigEff KfId

getConfig
  :: forall effs
  .  (Member ConfigEff effs)
  => Eff effs Config
getConfig = send GetConfig

genericLambda
  :: forall a b resEffs
  .  (Member ConfigEff resEffs, FromJSON a, ToJSON b)
  => Text
  -> (forall effs . (Member GenEff effs, Member S3Eff effs) => a -> Eff effs b)
  -> LambdaProfile
  -> Eff resEffs LambdaId
genericLambda name f =
  send . RegGenericLambda (Proxy :: Proxy a) (Proxy :: Proxy b) name f

s3Bucket
  :: (Member ConfigEff effs)
  => Text
  -> S3BucketProfile
  -> Eff effs S3BucketId
s3Bucket =
  send .: RegS3Bucket

s3BucketLambda
  :: forall resEffs
  .  (Member ConfigEff resEffs)
  => Text
  -> S3BucketId
  -> (forall effs . (Member GenEff effs, Member S3Eff effs) => S3LambdaProgram effs)
  -> LambdaProfile
  -> Eff resEffs LambdaId
s3BucketLambda name bucketId f =
  send . RegS3BucketLambda name bucketId f

s3Kf
  :: forall resEffs
  .  (Member ConfigEff resEffs)
  => Text
  -> S3BucketId
  -> Eff resEffs KfId
s3Kf name =
  send . RegS3BucketKf name
