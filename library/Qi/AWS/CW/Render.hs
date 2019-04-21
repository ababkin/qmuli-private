{-# LANGUAGE OverloadedLists   #-}

module Qi.AWS.CW.Render where

import           Control.Lens
import           Data.Aeson   
import           Protolude             hiding (all)
import           Stratosphere
import           Data.Hashable

import           Qi.AWS.Resource
import           Qi.Config
import           Qi.AWS.Types
import           Qi.AWS.CW


toResources :: Config -> Resources
toResources config@Config{ _appName } = Resources $ toResource <$> all config
  where
    toResource (lid, rule@CwEventsRule{ _cerLbdId }) =
      resource (show lid) $
        eventsRule
        & erName ?~ Literal (show pid)
        & erScheduleExpression ?~ Literal (rule ^. cerProfile . csepSchedule)
        & erState ?~ Literal ENABLED
        & erTargets ?~ [ target ]

      where
        target = eventsRuleTarget
                        tarn
                        (Literal tid)
        -- composite ID for the specific rule target (concat of the rule's and the lambda's pids)
        -- TODO: rethink this
        tid  = show $ hash (show pid <> "__" <> show lbdPid :: Text)
        tarn = GetAtt (show _cerLbdId) "Arn"
        pid  = toPhysicalId _appName lid
        lbdPid  = toPhysicalId _appName _cerLbdId
