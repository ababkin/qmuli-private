{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Qi.AWS.KF where

import           Control.Lens
import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Default        (Default, def)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as SHM
import           GHC.Show            (Show (..))
import           Protolude
import           Qi.AWS.Types
import Qi.AWS.S3


type KfId = LogicalId 'KinesisFirehoseResource

data KfConfig = KfConfig {
    _kfIdToKf :: HashMap KfId Kf
  }
  deriving (Eq, Show)
instance Default KfConfig where
  def = KfConfig {
      _kfIdToKf     = SHM.empty
    }

data Kf = Kf {
    _kfProfile :: KfProfile
  , _kfBucket :: S3BucketId
  }
  deriving (Eq, Show)
-- instance Default Kf where
--   def = Kf {
--       _kfName         = "default"
--     , _kfProfile      = def
--     }

data KfProfile = KfProfile
  { _kfpExistence    :: ResourceExistence
  }
  deriving (Eq, Show)
instance Default KfProfile where
  def = KfProfile
    { _kfpExistence = ShouldCreate
    }

makeLenses ''KfConfig
makeLenses ''Kf
makeLenses ''KfProfile
