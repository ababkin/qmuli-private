{-# LANGUAGE TemplateHaskell     #-}

module Qi.AWS.IAM where


import           Control.Lens
import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Default        (Default, def)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as SHM
import           GHC.Show            (Show (..))
import           Protolude
import           Qi.AWS.Types
import           Qi.AWS.ARN


type RoleId = LogicalId 'IamRoleResource

data IamConfig = IamConfig {
    _idToRole :: HashMap RoleId IamRole
  }
  deriving (Eq, Show)
instance Default IamConfig where
  def = IamConfig {
      _idToRole = SHM.empty
    }

data IamRole = IamRole {
    principalArn :: Arn
  }
  deriving (Eq, Show)

makeLenses ''IamConfig
