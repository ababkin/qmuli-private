{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Qi.Util where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson                 (Result (Error, Success),
                                             Value (Number, Object, String),
                                             encode, object)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.HashMap.Strict        (HashMap)
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Protolude
import           System.Console.ANSI
import qualified System.Process             as P


printSuccess
  :: MonadIO m
  => Text
  -> m ()
printSuccess = printVivid Green

printPending
  :: MonadIO m
  => Text
  -> m ()
printPending = printVivid Yellow

printVivid
  :: MonadIO m
  => Color
  -> Text
  -> m ()
printVivid color s = liftIO $ do
  setSGR [SetColor Foreground Vivid color]
  putStr s
  setSGR [Reset]
  putStr ("\n" :: Text)



getCurrentMilliseconds :: IO Int
getCurrentMilliseconds = (fromIntegral :: Integer -> Int) . round . (* 1000) <$> getPOSIXTime

time
  :: MonadIO m
  => [Char]
  -> m b
  -> m b
time label action = do
      before <- liftIO getCurrentMilliseconds
      x <- action
      liftIO $ do
        after <- getCurrentMilliseconds
        putStr $ label ++ " - turnaround in milliseconds: "
        print (after - before)
      return x

callProcess :: [Char] -> [[Char]] -> IO ()
callProcess pname args = do
  printSuccess $ "running '" <> toS pname <> "' with args: " <> show args <> " ..."
  P.callProcess pname args
