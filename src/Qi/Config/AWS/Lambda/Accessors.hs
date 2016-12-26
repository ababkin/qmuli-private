{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Config.AWS.Lambda.Accessors where

import           Control.Lens
import qualified Data.HashMap.Strict  as SHM
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda (Lambda, lbdName, lcLambdas)
import           Qi.Config.Identifier


rType = "lambda"
getName = (^.lbdName)
getMap = (^.lbdConfig.lcLambdas)


getLogicalNameFromId
  :: LambdaId
  -> Config
  -> Text
getLogicalNameFromId rid =
  getLogicalName . getById rid

getLogicalName
  :: Lambda
  -> Text
getLogicalName r =
  T.concat [getName r, "Lambda"]

getPhysicalName
  :: Lambda
  -> Config
  -> Text
getPhysicalName r config =
  getName r `underscoreNamePrefixWith` config

getPermissionLogicalName
  :: Lambda
  -> Text
getPermissionLogicalName r =
  T.concat [getName r, "LambdaPermission"]

getAll
  :: Config
  -> [Lambda]
getAll = SHM.elems . getMap

getById
  :: LambdaId
  -> Config
  -> Lambda
getById rid config =
  fromMaybe
    (error $ "Could not reference " ++ rType ++ " with id: " ++ show rid)
    $ SHM.lookup rid $ getMap config





