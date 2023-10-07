module Qi.AWS.Logger
  ( Logger,
    mkLambdaLogger,
    mkCliLogger,
  )
where

import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.ByteString.Builder as Build
import Amazonka.Logger (Logger)
import Protolude
import qualified Data.Text as T
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stderr,
  )

mkLambdaLogger ::
  IO Logger
mkLambdaLogger = do
  hSetBuffering stderr LineBuffering
  pure $ \lvl _b ->
    -- hPutStrLn stderr . encode $ object ["level" .= String (show lvl), "message" .= String (T.pack . toS $ Build.toLazyByteString b)]
    hPutStrLn stderr . encode $ object ["level" .= String (show lvl), "message" .= String "TODO: fix logging!"]

mkCliLogger ::
  IO Logger
mkCliLogger = do
  hSetBuffering stderr LineBuffering
  pure $ \_lvl b ->
    hPutStrLn stderr $ Build.toLazyByteString b
