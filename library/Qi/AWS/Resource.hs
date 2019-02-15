{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ScopedTypeVariables          #-}

module Qi.AWS.Resource where

import           Control.Lens hiding (mapping)
import           Data.Char            (isAlphaNum)
import           Data.Default         (Default, def)
import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as SHM
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Show             (Show (..))
import           Protolude            hiding (show)
import qualified Protolude            as P
import           Qi.AWS.Types
import           Qi.Config.AWS


class (Show (LogicalId (ResourceType r)), Hashable (LogicalId (ResourceType r))) => AwsResource r where

  type ResourceType r :: AwsResourceType

  typeName
    :: r
    -> Text

  name
    :: Config
    -> r
    -> Text

  mapping
    :: Config
    -> SHM.HashMap (LogicalId (ResourceType r)) r

  -- getAllWithIds
  --   :: Config
  --   -> [(rid, r)]
  -- getAllWithIds = SHM.toList . getMap

  all
    :: Config
    -> [ r ]
  all = SHM.elems . mapping

  getById
    :: Config
    -> LogicalId (ResourceType r)
    -> r
  getById config lid =
    fromMaybe
      (panic $ "Could not reference resource with logical id: " <> P.show lid)
      $ SHM.lookup lid $ mapping config

  logicalId
    :: Config
    -> r
    -> LogicalId (ResourceType r)
  logicalId config r =
    LogicalId $ makeAlphaNumeric (name config r) <> typeName r

  physicalId
    :: Config
    -> r
    -> PhysicalId (ResourceType r)
  -- getPhysicalName config r =
  --   PhysicalName $ makeAlphaNumeric (getName config r) `underscoreNamePrefixWith` config

  -- getLogicalNameFromId
  --   :: Config
  --   -> rid
  --   -> LogicalName r
  -- getLogicalNameFromId config rid =
  --   getLogicalName config $ getById config rid

  {- class Conv a b where -}
    {- conv :: Config -> a -> b -}

    {- instance Conv rid r where -}
      {- conv = getById -}

    {- instance Conv rid (PhysicalName r) where -}
      {- conv config = getPhysicalName config . getById config -}



-- instance CfResource Lambda LambdaId where
--   rNameSuffix = const "Lambda"
--   getName _ = (^. lbdName)
--   getMap = (^. lbdConfig . lbdIdToLambda)


-- instance CfResource S3Bucket S3BucketId where
--   rNameSuffix = const "S3Bucket"
--   getName _ = (^. s3bName)
--   getMap = (^. s3Config . s3IdToBucket)
--   getPhysicalName config r =
--     PhysicalName $ makeAlphaNumeric (getName config r) `dotNamePrefixWith` config


-- instance CfResource Kf KfId where
--   rNameSuffix = const "Kf"
--   getName _ = (^. kfName)
--   getMap = (^. kfConfig . kfIdToKf)
--   getPhysicalName config r =
--     PhysicalName $ makeAlphaNumeric (getName config r) `dotNamePrefixWith` config
