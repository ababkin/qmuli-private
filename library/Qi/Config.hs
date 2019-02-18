{-# LANGUAGE TemplateHaskell #-}

module Qi.Config ( Config (..)
                 , appName
                 , s3Config
                 , lbdConfig
                 , kfConfig
                 , mkConfig
                 ) where

import           Control.Lens
import           Data.Char           (isAlphaNum)
import           Data.Default        (Default, def)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as SHM
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           GHC.Show            (Show (..))
import           Protolude           hiding (show)
import qualified Protolude           as P
import           Qi.AWS.KF
import           Qi.AWS.Lambda
import           Qi.AWS.S3
import           Qi.AWS.Types


data Config = Config {
    _appName   :: AppName
  , _s3Config  :: S3Config
  , _lbdConfig :: LambdaConfig
  , _kfConfig  :: KfConfig
}
  deriving (Eq, Show)

mkConfig :: AppName -> Config
mkConfig appName =
  Config {
      _appName = appName
    , _s3Config   = def
    , _lbdConfig  = def
    , _kfConfig   = def
  }

makeLenses ''Config

-- underscoreNamePrefixWith
--   :: Text
--   -> Config
--   -> Text
-- underscoreNamePrefixWith = namePrefixWith "_"

-- dotNamePrefixWith
--   :: Text
--   -> Config
--   -> Text
-- dotNamePrefixWith = namePrefixWith "."

-- namePrefixWith
--   :: Text
--   -> Text
--   -> Config
--   -> Text
-- namePrefixWith sep name config =
--   T.concat [show $ config ^. appName, sep, name]


-- makeAlphaNumeric
--   :: Text
--   -> Text
-- makeAlphaNumeric = T.filter isAlphaNum
