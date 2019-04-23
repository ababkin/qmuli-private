{-# LANGUAGE TemplateHaskell     #-}

module Qi.Program.KF.Lang where

import           Data.Aeson          (ToJSON)
import           Protolude
import           Polysemy

import           Qi.AWS.S3
import           Qi.AWS.KF
import           Qi.AWS.Types


data KfEff m r where

  Put
    :: ToJSON a
    => KfStreamId
    -> a
    -> KfEff m ()

makeSem ''KfEff
