-- |

module Qi.AWS.Render where

import qualified Data.ByteString.Lazy           as LBS
import           Protolude
import qualified Qi.AWS.IAMRole.Render          as Role
import qualified Qi.AWS.KF.Render               as KF
import qualified Qi.AWS.Lambda.Render           as Lambda
import qualified Qi.AWS.LambdaPermission.Render as LambdaPermission
import qualified Qi.AWS.S3.Render               as S3
import qualified Qi.AWS.CW.Render               as CW
import qualified Qi.AWS.SQS.Render              as SQS
import           Qi.Config
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
  , LambdaPermission.toResources
  , Lambda.toResources
  , KF.toResources
  , CW.toResources
  , SQS.toResources
  ]
