{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Qi.AWS.Lambda.Function where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Default (Default, def)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as SHM
import Data.Proxy (Proxy)
import Data.Text (Text)
import GHC.Show (Show (..))
import Polysemy
import Protolude as P
-- hiding (lfRole, LambdaFunction)

import Qi.AWS.ARN
import Qi.AWS.IAM
import Qi.AWS.Renderable
import Qi.AWS.Service
import Qi.AWS.Types
import Qi.Program.Gen.Lang
import Qi.Program.Lambda.Lang (LambdaEff)
import Qi.Program.S3.Lang (S3Eff)
import Stratosphere (Val (..))
import qualified Stratosphere as S

type AllLambdaEffects effs =
  ( Member GenEff effs,
    Member S3Eff effs,
    Member LambdaEff effs
  )

-- We hide the in ond out types from the LambdaFunction type
-- because we want to store functions with different in and out
-- types in the same container
data LambdaFunction = forall a b.
  (FromJSON a, ToJSON b) =>
  LambdaFunction
  { roleId :: RoleId,
    profile :: LambdaFunctionProfile,
    inType :: Proxy a,
    outType :: Proxy b,
    program ::
      forall effs.
      AllLambdaEffects effs =>
      a ->
      Sem effs b
  }

instance Eq LambdaFunction where
  _ == _ = True -- TODO: do something about this aweful hack

instance Show LambdaFunction where
  show LambdaFunction {} = "LambdaFunction"

instance AwsResource LambdaFunction where
  type ResourceType LambdaFunction = 'LambdaFunctionResource

data LambdaFunctionProfile = LambdaFunctionProfile
  { _lfpMemorySize :: LambdaFunctionMemorySize,
    _lfpTimeoutSeconds :: Int
  }
  deriving (Eq, Show)

data LambdaFunctionMemorySize
  = M128
  | M192
  | M256
  | M320
  | M384
  | M448
  | M512
  | M1024
  | M1536
  | M2048
  | M2560
  | M3008
  deriving (Eq, Show)

instance Enum LambdaFunctionMemorySize where
  toEnum 128 = M128
  toEnum 192 = M192
  toEnum 256 = M256
  toEnum 320 = M320
  toEnum 384 = M384
  toEnum 448 = M448
  toEnum 512 = M512
  toEnum 1024 = M1024
  toEnum 1536 = M1536
  toEnum 2048 = M2048
  toEnum 2560 = M2560
  toEnum 3008 = M3008
  toEnum x = panic $ "no such memory configuration: " <> P.show x

  fromEnum M128 = 128
  fromEnum M192 = 192
  fromEnum M256 = 256
  fromEnum M320 = 320
  fromEnum M384 = 384
  fromEnum M448 = 448
  fromEnum M512 = 512
  fromEnum M1024 = 1024
  fromEnum M1536 = 1536
  fromEnum M2048 = 2048
  fromEnum M2560 = 2560
  fromEnum M3008 = 3008

instance Default LambdaFunctionProfile where
  def =
    LambdaFunctionProfile
      { _lfpMemorySize = M128,
        _lfpTimeoutSeconds = 30
      }

makeLenses ''LambdaFunctionProfile

-- toResources :: Config -> Resources
-- toResources config@Config{ _appName } = Resources $ map toLambdaResource lbds
--   where
--     lbds :: [ (LambdaId, LambdaFunction) ] = all config

instance Renderable LambdaFunction where
  render appName (lid, LambdaFunction {roleId, profile}) =
    ( S.resource (P.show lid) $
        S.lambdaFunction
          lbdCode
          "index.handler"
          (GetAtt (P.show roleId) "Arn")
          (Literal $ S.OtherRuntime "provided")
          & S.lfFunctionName ?~ Literal (P.show $ toPhysicalId appName lid)
          & S.lfMemorySize ?~ Literal memorySize
          & S.lfTimeout ?~ Literal timeOut
    )
    where
      memorySize = fromIntegral . fromEnum $ profile ^. lfpMemorySize
      timeOut = fromIntegral $ profile ^. lfpTimeoutSeconds

      lbdCode =
        S.lambdaFunctionCode
          & S.lfcS3Bucket ?~ lambdaS3Bucket
          & S.lfcS3Key ?~ lambdaS3Object

      lambdaS3Bucket :: Val Text
      lambdaS3Bucket = Literal $ P.show appName <> ".app"

      lambdaS3Object :: Val Text
      lambdaS3Object = "lambda.zip"
