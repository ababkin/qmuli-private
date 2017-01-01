{-# LANGUAGE OverloadedStrings #-}

module Qi.Dispatcher (
    invokeLambda
  , deployApp
  , createCfStack
  , describeCfStack
  , destroyCfStack
  , go
  , renderCfTemplate
  ) where

import           Control.Lens
import           Control.Monad                 (void, (<=<))
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Data.Aeson.Encode.Pretty      (encodePretty)
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Network.AWS                   (AWS, send)
import           Prelude                       hiding (FilePath, log)
import           System.Environment.Executable (splitExecutablePath)
import           Turtle                        (FilePath, fromString, liftIO,
                                                sh, toText)

import qualified Qi.Amazonka                   as A
import           Qi.Config.AWS                 (Config, getAll, getPhysicalName,
                                                namePrefix)
import           Qi.Config.AWS.S3              (S3Bucket)
import qualified Qi.Config.CF                  as CF
import           Qi.Dispatcher.Build           (build)
import           Qi.Dispatcher.CF              (createStack, deleteStack,
                                                describeStack,
                                                waitOnStackCreated,
                                                waitOnStackDeleted)
import           Qi.Dispatcher.Lambda          (invokeLambda)
import           Qi.Dispatcher.S3              (clearBuckets, createBucket,
                                                putObject)
import           Qi.Util                       (printPending, printSuccess)


type Dispatcher = ReaderT Config IO

withConfig
  :: (Config -> Dispatcher ())
  -> Dispatcher ()
withConfig action = action =<< ask

withAppName
  :: (Text -> Dispatcher ())
  -> Dispatcher ()
withAppName action = withConfig $ action . (^.namePrefix)

runAmazonka
  :: AWS a
  -> Dispatcher a
runAmazonka = liftIO . A.runAmazonka

renderCfTemplate :: Dispatcher ()
renderCfTemplate =
   withConfig $ liftIO . LBS.putStr . CF.render

deployApp :: Dispatcher ()
deployApp =
  withConfig $ \config -> do
    let appName = config^.namePrefix

    content <- liftIO $ do
      (_, execFilename) <- splitExecutablePath -- get the current executable filename
      lambdaPackagePath <- fromString <$> build "." execFilename
      LBS.readFile . T.unpack $ toTextIgnore lambdaPackagePath

    runAmazonka $ do
      createBucket appName
      putObject appName "cf.json" $ CF.render config
      putObject appName "lambda.zip" content

  where
    toTextIgnore :: FilePath -> T.Text
    toTextIgnore x = case toText x of
      Right s -> s
      Left _  -> ""


createCfStack :: Dispatcher ()
createCfStack =
  withAppName $ \appName -> do
    printSuccess "creating the stack..."
    runAmazonka $ createStack appName
    printPending "waiting on the stack to be created..."
    liftIO $ waitOnStackCreated appName
    printSuccess "stack was successfully created"


describeCfStack :: Dispatcher ()
describeCfStack =
  withAppName $ liftIO . LBS.putStrLn . encodePretty
                  <=< runAmazonka . describeStack


destroyCfStack
  :: Dispatcher ()
  -> Dispatcher ()
destroyCfStack action =
  withConfig $ \config -> do
    let appName = config^.namePrefix

    printSuccess "destroying the stack..."
    runAmazonka $ do
      clearBuckets $ map (getPhysicalName config) (getAll config :: [S3Bucket])
      deleteStack appName

    action

    printPending "waiting on the stack to be destroyed..."
    liftIO $ waitOnStackDeleted appName
    printSuccess "stack was successfully destroyed"


go :: Dispatcher ()
go = do
    destroyCfStack $ do
      printSuccess "deploying the app..."
      deployApp
    createCfStack
    printSuccess "all done!"




