module Qi.AWS.SQS.Render where

import           Control.Lens
import           Protolude         hiding (all)
import  Stratosphere

import           Qi.AWS.SQS
import           Qi.Config
import           Qi.AWS.Resource
import           Qi.AWS.Types


toResources
  :: Config
  -> Resources
toResources config@Config{ _appName } = Resources $ toResource <$> all config
  where
    toResource (lid, queue@SqsQueue{  }) =
      let
        pid = toPhysicalId _appName lid
      in
        resource (show lid) $
          sqsQueue
          & sqsqQueueName ?~ Literal (show pid)
