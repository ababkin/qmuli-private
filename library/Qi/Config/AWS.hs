{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Qi.Config.AWS where

import           Control.Lens
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
import           Qi.Config.AWS.KF
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3
import           Qi.Config.Identifier


data Config = Config {
    _namePrefix   :: Text
  , _nextId       :: Int
  , _waitOnLogger :: Bool
  , _s3Config     :: S3Config
  , _lbdConfig    :: LambdaConfig
  , _kfConfig     :: KfConfig
}
  deriving (Eq, Show)

instance Default Config where
  def = Config {
      _namePrefix   = "qmuli"
    , _nextId       = 0  -- global autoincrement id state
    , _waitOnLogger = True
    , _s3Config     = def
    , _lbdConfig    = def
    , _kfConfig    = def
  }

makeLenses ''Config



underscoreNamePrefixWith
  :: Text
  -> Config
  -> Text
underscoreNamePrefixWith = namePrefixWith "_"

dotNamePrefixWith
  :: Text
  -> Config
  -> Text
dotNamePrefixWith = namePrefixWith "."

namePrefixWith
  :: Text
  -> Text
  -> Config
  -> Text
namePrefixWith sep name config =
  T.concat [config ^. namePrefix, sep, name]


makeAlphaNumeric
  :: Text
  -> Text
makeAlphaNumeric = T.filter isAlphaNum

data LogicalName r = LogicalName { unLogicalName :: Text }
  deriving Eq
instance Show (LogicalName r) where
  show (LogicalName ln) = toS ln

data PhysicalName r = PhysicalName { unPhysicalName :: Text }
  deriving Eq
instance Show (PhysicalName r) where
  show (PhysicalName ln) = toS ln



class (Eq rid, Show rid, Hashable rid) => CfResource r rid | rid -> r, r -> rid where

  rNameSuffix
    :: r
    -> Text
  getName
    :: Config
    -> r
    -> Text
  getMap
    :: Config
    -> SHM.HashMap rid r

  getAllWithIds
    :: Config
    -> [(rid, r)]
  getAllWithIds = SHM.toList . getMap

  getAll
    :: Config
    -> [r]
  getAll = SHM.elems . getMap

  getById
    :: Config
    -> rid
    -> r
  getById config rid =
    fromMaybe
      (panic $ "Could not reference resource with id: " <> P.show rid)
      $ SHM.lookup rid $ getMap config

  getLogicalName
    :: Config
    -> r
    -> LogicalName r
  getLogicalName config r =
    LogicalName $ T.concat [makeAlphaNumeric (getName config r), rNameSuffix r]

  getPhysicalName
    :: Config
    -> r
    -> PhysicalName r
  getPhysicalName config r =
    PhysicalName $ makeAlphaNumeric (getName config r) `underscoreNamePrefixWith` config

  getLogicalNameFromId
    :: Config
    -> rid
    -> LogicalName r
  getLogicalNameFromId config rid =
    getLogicalName config $ getById config rid

  {- class Conv a b where -}
    {- conv :: Config -> a -> b -}

    {- instance Conv rid r where -}
      {- conv = getById -}

    {- instance Conv rid (PhysicalName r) where -}
      {- conv config = getPhysicalName config . getById config -}



instance CfResource Lambda LambdaId where
  rNameSuffix = const "Lambda"
  getName _ = (^. lbdName)
  getMap = (^. lbdConfig . lbdIdToLambda)


instance CfResource S3Bucket S3BucketId where
  rNameSuffix = const "S3Bucket"
  getName _ = (^. s3bName)
  getMap = (^. s3Config . s3IdToBucket)
  getPhysicalName config r =
    PhysicalName $ makeAlphaNumeric (getName config r) `dotNamePrefixWith` config


instance CfResource Kf KfId where
  rNameSuffix = const "Kf"
  getName _ = (^. kfName)
  getMap = (^. kfConfig . kfIdToKf)
  getPhysicalName config r =
    PhysicalName $ makeAlphaNumeric (getName config r) `dotNamePrefixWith` config



