module Qi.Program.KF.Lang where

import           Control.Monad.Freer
import           Data.Aeson          (ToJSON)
import           Protolude
import           Qi.AWS.S3
import           Qi.AWS.KF
import           Qi.AWS.Types
import           Qi.Core.Curry


data KfEff r where

  Put
    :: ToJSON a
    => KfId
    -> a
    -> KfEff ()

put
  :: (Member KfEff effs, ToJSON a)
  => KfId
  -> a
  -> Eff effs ()
put id =
  send . Put id
