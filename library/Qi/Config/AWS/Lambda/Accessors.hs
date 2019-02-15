{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.Lambda.Accessors where

import           Control.Lens
import qualified Data.HashMap.Strict  as SHM
import           Protolude
import           Qi.AWS.Resource
import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda


getPermissionLogicalName
  :: Config
  -> Lambda
  -> Text
getPermissionLogicalName config r =
  name config r <> "LambdaPermission"
