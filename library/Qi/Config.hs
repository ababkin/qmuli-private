{-# LANGUAGE TemplateHaskell #-}

module Qi.Config
  ( Config (..),
    appName,
    s3Config,
    lbdConfig,
    kfConfig,
    iamConfig,
    cwConfig,
    sqsConfig,
    mkConfig,
    Configable (..),
  )
where

import Control.Lens (makeLenses, view)
import Data.Default (def)
import qualified Data.HashMap.Strict as SHM
import Protolude
import Qi.AWS.CW
import Qi.AWS.IAM
import Qi.AWS.KF
import Qi.AWS.Lambda
import Qi.AWS.Lambda.EventSourceMapping
import Qi.AWS.Lambda.Function
import Qi.AWS.Lambda.Permission
import Qi.AWS.S3
import Qi.AWS.SQS
import Qi.AWS.Types

data Config = Config
  { _appName :: AppName,
    _s3Config :: S3Config,
    _lbdConfig :: LambdaConfig,
    _kfConfig :: KfConfig,
    _iamConfig :: IamConfig,
    _cwConfig :: CwConfig,
    _sqsConfig :: SqsConfig
  }
  deriving (Eq, Show)

mkConfig :: AppName -> Config
mkConfig appName =
  Config
    { _appName = appName,
      _s3Config = def,
      _lbdConfig = def,
      _kfConfig = def,
      _iamConfig = def,
      _cwConfig = def,
      _sqsConfig = def
    }

makeLenses ''Config

-- TODO: only render non-existing resources
--     buckets = filter (\(_, s3b) -> s3b ^. s3bProfile . s3bpExistence /= AlreadyExists) $ all config

class
  ( AwsResource r,
    Show (LogicalId (ResourceType r)),
    Hashable (LogicalId (ResourceType r))
  ) =>
  Configable r
  where
  mapping ::
    Config ->
    SHM.HashMap (LogicalId (ResourceType r)) r

  all ::
    Config ->
    [(LogicalId (ResourceType r), r)]
  all = SHM.toList . mapping

  getById ::
    Config ->
    LogicalId (ResourceType r) ->
    r
  getById config lid =
    fromMaybe
      (panic $ "Could not reference resource with logical id: " <> show lid)
      $ SHM.lookup lid $ mapping config

instance Configable LambdaFunction where
  mapping = view $ lbdConfig . idToFunction

instance Configable LambdaPermission where
  mapping = view $ lbdConfig . idToPermission

instance Configable LambdaEventSourceMapping where
  mapping = view $ lbdConfig . idToEventSourceMapping

instance Configable KfStream where
  mapping = view $ kfConfig . idToStream

instance Configable S3Bucket where
  mapping = view $ s3Config . idToBucket

instance Configable IamRole where
  mapping = view $ iamConfig . idToRole

instance Configable CwEventsRule where
  mapping = view $ cwConfig . idToRule

instance Configable SqsQueue where
  mapping = view $ sqsConfig . idToQueue
