{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Qi.Program.Lambda.Lang where

import           Control.Monad.Freer
import           Data.Aeson          (ToJSON)
import           Protolude
import           Qi.AWS.S3           (S3Object)
import           Qi.AWS.Types
import           Qi.Core.Curry


type LambdaId = LogicalId 'LambdaResource

data LambdaEff r where

  Invoke
    :: ToJSON a
    => LambdaId
    -> a
    -> LambdaEff ()

  Update
    :: LambdaId
    -> S3Object
    -> LambdaEff ()


invoke
  :: (Member LambdaEff effs, ToJSON a)
  => LambdaId
  -> a
  -> Eff effs ()
invoke =
  send .: Invoke


update
  :: (Member LambdaEff effs)
  => LambdaId
  -> S3Object
  -> Eff effs ()
update =
  send .: Update

