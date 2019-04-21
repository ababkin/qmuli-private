{-# LANGUAGE TemplateHaskell     #-}

module Qi.Program.KF.Lang where

import           Data.Aeson          (ToJSON)
import           Protolude
import           Data.Composition
import           Polysemy

import           Qi.AWS.S3
import           Qi.AWS.KF
import           Qi.AWS.Types


data KfEff m r where

  Put
    :: ToJSON a
    => KfId
    -> a
    -> KfEff m ()

makeSem ''KfEff

-- put
--   :: (Member KfEff effs, ToJSON a)
--   => KfId
--   -> a
--   -> Eff effs ()
-- put id =
--   send . Put id
