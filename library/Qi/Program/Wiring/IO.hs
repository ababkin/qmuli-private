{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Qi.Program.Wiring.IO  where

import           Control.Monad.Freer
import           Control.Monad.Freer.State
import           Protolude                     hiding (State, runState, (<&>))
import           Qi.AWS.Logger                 (Logger)
import           Qi.AWS.Types                  (AwsMode (..))
import           Qi.Config
import qualified Qi.Program.CF.Ipret.Gen       as CF
import           Qi.Program.CF.Lang            (CfEff)
import qualified Qi.Program.Config.Ipret.State as Config
import           Qi.Program.Config.Lang        (ConfigEff)
import qualified Qi.Program.Gen.Ipret.IO       as Gen
import           Qi.Program.Gen.Lang           (GenEff)
import qualified Qi.Program.Lambda.Ipret.Gen   as Lbd
import           Qi.Program.Lambda.Lang        (LambdaEff)
import qualified Qi.Program.S3.Ipret.Gen       as S3
import           Qi.Program.S3.Lang            (S3Eff)
import qualified Qi.Program.KF.Ipret.Gen       as Kf
import           Qi.Program.KF.Lang            (KfEff)


run
  :: Config
  -> AwsMode
  -> IO Logger
  -> (Eff '[ CfEff
           , S3Eff
           , KfEff
           , LambdaEff
           , GenEff
           , ConfigEff
           , State Config
           , IO
           ] a -> IO a)
run config awsMode mkLogger =
    runM
  . map fst
  . runState config
  . Config.run
  . Gen.run awsMode mkLogger
  . Lbd.run
  . Kf.run
  . S3.run
  . CF.run
