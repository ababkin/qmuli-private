module Qi.AWS.Logger
  ( Logger,
    mkLambdaLogger,
    mkCliLogger,
  )
where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.ByteString.Lazy.Builder as Build
import Amazonka.Logger (Logger)
import Protolude
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stderr,
  )

mkLambdaLogger ::
  IO Logger
mkLambdaLogger = do
  hSetBuffering stderr LineBuffering
  pure $ \lvl b ->
    hPutStrLn stderr . encode $ object ["level" .= String (show lvl), "message" .= String (toS $ Build.toLazyByteString b)]

mkCliLogger ::
  IO Logger
mkCliLogger = do
  hSetBuffering stderr LineBuffering
  pure $ \_lvl b ->
    hPutStrLn stderr $ Build.toLazyByteString b
