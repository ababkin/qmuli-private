{-# LANGUAGE TemplateHaskell #-}

module Qi.Config ( Config (..)
                 , appName
                 , s3Config
                 , lbdConfig
                 , kfConfig
                 , iamConfig
                 , cwConfig
                 , mkConfig
                 ) where

import           Control.Lens (makeLenses)
import           Data.Default        (def)
import           Qi.AWS.KF
import           Qi.AWS.Lambda
import           Qi.AWS.S3
import           Qi.AWS.Types
import           Qi.AWS.IAM
import           Qi.AWS.CW
import           Protolude


data Config = Config {
    _appName   :: AppName
  , _s3Config  :: S3Config
  , _lbdConfig :: LambdaConfig
  , _kfConfig  :: KfConfig
  , _iamConfig :: IamConfig
  , _cwConfig  :: CwConfig

}
  deriving (Eq, Show)

mkConfig :: AppName -> Config
mkConfig appName =
  Config {
      _appName = appName
    , _s3Config   = def
    , _lbdConfig  = def
    , _kfConfig   = def
    , _iamConfig  = def
    , _cwConfig   = def
  }

makeLenses ''Config
