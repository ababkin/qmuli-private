{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Qi.AWS.Render where

import           Control.Lens              hiding (view, mapping)
import qualified Data.ByteString.Lazy           as LBS
import           Protolude hiding (all)
import  qualified         Stratosphere as S

import  Qi.AWS.Lambda.Function
import  Qi.AWS.Lambda.Permission
import  Qi.AWS.Lambda.EventSourceMapping
import  Qi.AWS.IAM
import  Qi.AWS.KF
import  Qi.AWS.S3
import  Qi.AWS.CW
import  Qi.AWS.SQS
import           Qi.Config
import qualified Qi.AWS.Renderable as R


render
  :: Config
  -> LBS.ByteString
render config = S.encodeTemplate $
  S.template
    (toAllResources config)
    & S.templateDescription ?~ "Example"
    & S.templateFormatVersion ?~ "2010-09-09"
    -- & S.templateOutputs ?~ [] --toOutputs config


toResources
  :: forall a
  .  (R.Renderable a, Configable a)
  => Config
  -> S.Resources
toResources config@Config{ _appName } =
  S.Resources $ (R.render @a) _appName <$> all config

toAllResources
  :: Config
  -> S.Resources
toAllResources config = mconcat $ ($ config) <$>
  [ toResources @S3Bucket
  , toResources @IamRole
  , toResources @LambdaPermission
  , toResources @LambdaEventSourceMapping
  , toResources @LambdaFunction
  , toResources @KfStream
  , toResources @CwEventsRule
  , toResources @SqsQueue
  ]
