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
import           Qi.AWS.Types
import           Qi.Config.AWS.KF
import           Qi.Config.AWS.Lambda
import           Qi.Config.AWS.S3


data Config = Config {
    _namePrefix :: Text
  , _s3Config   :: S3Config
  , _lbdConfig  :: LambdaConfig
  , _kfConfig   :: KfConfig
}
  deriving (Eq, Show)

instance Default Config where
  def = Config {
      _namePrefix   = "qmuli"
    , _s3Config     = def
    , _lbdConfig    = def
    , _kfConfig  = def
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



