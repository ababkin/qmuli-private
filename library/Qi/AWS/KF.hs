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
import           Qi.AWS.IAM
import Qi.AWS.S3


type KfStreamId = LogicalId 'KfStreamResource

data KfConfig = KfConfig {
    _kfIdToStream :: HashMap KfStreamId KfStream
  }
  deriving (Eq, Show)
instance Default KfConfig where
  def = KfConfig {
      _kfIdToStream     = SHM.empty
    }
-- TODO: parameterize the dest
data KfStream = KfStream {
    _kfProfile :: KfStreamProfile
  , _kfRole    :: RoleId
  , _kfBucket  :: S3BucketId
  }
  deriving (Eq, Show)

data KfStreamProfile = KfStreamProfile
  { _kfpExistence    :: ResourceExistence
  }
  deriving (Eq, Show)
instance Default KfStreamProfile where
  def = KfStreamProfile
    { _kfpExistence = ShouldCreate
    }

makeLenses ''KfConfig
makeLenses ''KfStream
makeLenses ''KfStreamProfile
