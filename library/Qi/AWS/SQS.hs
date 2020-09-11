{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Qi.AWS.SQS where

import Control.Lens
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Default (Default, def)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as SHM
import Protolude
import Qi.AWS.Renderable
import Qi.AWS.Types
import Stratosphere

data SqsEvent = SqsEvent
  {
  }
  deriving (Eq, Show)

instance FromJSON SqsEvent where
  parseJSON v = pure SqsEvent

data SqsQueue = SqsQueue
  {
  }
  deriving (Eq, Show)

instance AwsResource SqsQueue where
  type ResourceType SqsQueue = 'SqsQueueResource

instance Renderable SqsQueue where
  render appName (lid, queue@SqsQueue {}) =
    let pid = toPhysicalId appName lid
     in resource (show lid) $
          sqsQueue
            & sqsqQueueName ?~ Literal (show pid)

data SqsConfig = SqsConfig
  { _idToQueue :: HashMap QueueId SqsQueue
  }
  deriving (Eq, Show)

instance Default SqsConfig where
  def =
    SqsConfig
      { _idToQueue = SHM.empty
      }

makeLenses ''SqsConfig
