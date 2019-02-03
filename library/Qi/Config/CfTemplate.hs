module Qi.Config.CfTemplate (render) where

import qualified Data.ByteString.Lazy    as LBS
import           Protolude
import           Qi.Config.AWS
import qualified Qi.Config.Render.KF     as KF
import qualified Qi.Config.Render.Lambda as Lambda
import qualified Qi.Config.Render.Role   as Role
import qualified Qi.Config.Render.S3     as S3
import           Stratosphere


render
  :: Config
  -> LBS.ByteString
render config = encodeTemplate $
  template
    (toResources config)
    & templateDescription ?~ "Example"
    & templateFormatVersion ?~ "2010-09-09"
    -- & templateOutputs ?~ [] --toOutputs config

toResources
  :: Config
  -> Resources
toResources config = mconcat $ ($ config) <$>
  [ S3.toResources
  , Role.toResources
  , Lambda.toResources
  , KF.toResources
  ]
