{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Qi.Program.Wiring.IO  where

import           Control.Monad.Freer
import           Control.Monad.Freer.State
import           Protolude                     hiding (State, runState, (<&>))
import           Qi.AWS.Types                  (AwsMode (..), MkAwsLogger)
import           Qi.Config.AWS
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


run
  :: Config
  -> AwsMode
  -> MkAwsLogger
  -> (Eff '[CfEff, S3Eff, LambdaEff, GenEff, ConfigEff, State Config, IO] a -> IO a)
run config awsMode mkLogger =
    runM
  . map fst
  . runState config
  . Config.run
  . Gen.run awsMode mkLogger
  . Lbd.run
  . S3.run
  . CF.run
