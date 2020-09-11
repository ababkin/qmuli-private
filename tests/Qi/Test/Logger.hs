module Qi.Test.Logger where

import qualified Data.ByteString.Lazy.Builder as Build
import Protolude
import Qi.AWS.Logger (Logger)
import System.IO hiding (hPutStrLn)

mkTestLogger ::
  IO Logger
mkTestLogger = do
  hSetBuffering stderr LineBuffering
  pure $ \_lvl b ->
    hPutStrLn stderr $ Build.toLazyByteString b
