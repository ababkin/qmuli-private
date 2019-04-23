{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Qi.AWS.SQS where

import           Control.Lens
import           Data.ByteString      (ByteString)
import           Data.Default         (Default, def)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as SHM
import           GHC.Show             (Show (..))
import           Protolude

import           Qi.AWS.Types


type QueueId = LogicalId 'SqsQueueResource

data SqsQueue = SqsQueue {
  }
  deriving (Eq, Show)

data SqsConfig = SqsConfig {
    _idToQueue :: HashMap QueueId SqsQueue
  }
  deriving (Eq, Show)

instance Default SqsConfig where
  def = SqsConfig {
      _idToQueue = SHM.empty
    }

makeLenses ''SqsConfig
