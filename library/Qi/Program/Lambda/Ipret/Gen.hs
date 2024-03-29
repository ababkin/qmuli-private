{-# LANGUAGE OverloadedLists #-}

module Qi.Program.Lambda.Ipret.Gen (run) where

import Control.Lens (Getting, (.~), (?~), (^.))
import Data.Aeson (encode)
import Network.AWS.Lambda
  ( InvocationType (Event),
    iInvocationType,
    invoke,
    lambda,
    uS3Bucket,
    uS3Key,
    updateFunctionCode,
  )
import Network.AWS.S3 (ObjectKey (ObjectKey))
import Polysemy hiding (run)
import Protolude hiding ((<&>))
import Qi.AWS.Lambda
import Qi.AWS.S3
import Qi.AWS.Types
import Qi.Config
import Qi.Program.Config.Lang (ConfigEff, getConfig)
import Qi.Program.Gen.Lang
import Qi.Program.Lambda.Lang (LambdaEff (..))

run ::
  forall effs a.
  (Member GenEff effs, Member ConfigEff effs) =>
  (Sem (LambdaEff ': effs) a -> Sem effs a)
run =
  interpret
    ( \case
        Invoke lid payload -> do
          Config {_appName} <- getConfig
          void . amazonka lambda $
            invoke (show $ toPhysicalId _appName lid) (toS $ encode payload)
              & iInvocationType ?~ Event
        Update lid S3Object {_s3oBucketId, _s3oKey = S3Key s3Key} -> do
          Config {_appName} <- getConfig
          let pid = toPhysicalId _appName lid
          void . amazonka lambda $
            updateFunctionCode (show pid)
              & uS3Bucket ?~ show (toPhysicalId _appName _s3oBucketId)
              & uS3Key ?~ s3Key
    )
