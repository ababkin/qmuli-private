{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Qi.Program.Gen.Ipret.IO (run) where

import Control.Lens hiding ((.=))
import Control.Monad.Trans.AWS (runAWST)
import qualified Control.Monad.Trans.AWS as AWS (send)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Binary (sinkLbs)
import qualified Data.Time.Clock as C
import Network.AWS hiding (Request, Response, send)
import Network.AWS.Types (Service (..))
import Network.HTTP.Client (httpLbs, newManager)
import Polysemy hiding (run)
import Protolude hiding ((<&>))
import Qi.AWS.Types
import Qi.AWS.Types (AwsMode (..))
import Qi.Amazonka (currentRegion)
import Qi.Config (appName)
import Qi.Program.Config.Lang (ConfigEff, getConfig)
import Qi.Program.Gen.Lang (GenEff (..))
import Qi.Util (callProcess, printPending)
import Servant.Client hiding (Http)
import System.Build
  ( BuildArgs (SimpleTarget),
    stackInDocker,
  )
import System.Directory
  ( createDirectoryIfMissing,
    renameFile,
  )
import System.Docker (ImageName (ImageName))
import System.Environment.Executable (splitExecutablePath)
import System.IO (stderr)
import System.Posix.Files
import System.Posix.Types (FileMode)

run ::
  forall effs a.
  (Member (Embed IO) effs) =>
  AwsMode ->
  IO Logger ->
  (Sem (GenEff ': effs) a -> Sem effs a)
run mode mkLogger =
  interpret
    ( \act -> liftIO $ case act of
        Http mgrSettings req ->
          httpLbs req =<< newManager mgrSettings
        RunServant mgrSettings baseUrl req -> do
          mgr <- newManager mgrSettings
          runClientM req $ mkClientEnv mgr baseUrl
        Amazonka svc req ->
          runAmazonka svc $ AWS.send req
        AmazonkaPostBodyExtract svc req post ->
          runAmazonka svc $
            map Right . (`sinkBody` sinkLbs) . post =<< AWS.send req
        Say msg -> do
          hPutStrLn stderr . encode $ object ["message" .= String msg]
          putStrLn msg :: IO ()
        GetCurrentTime -> C.getCurrentTime
        Sleep us -> threadDelay us
        Build -> do
          (_, execFilename) <- splitExecutablePath -- get the current executable filename
          toS <$> build "." (toS execFilename)
        ReadFileLazy path ->
          LBS.readFile $ toS path
        PutStr content -> LBS.putStr content
    )
  where
    runAmazonka :: Service -> AWS b -> IO b
    runAmazonka svc action = do
      logger <- mkLogger
      env <-
        newEnv Discover
          <&> set envLogger logger
            . set envRegion currentRegion

      runResourceT . runAWST env $ reconf mode svc action

build ::
  FilePath ->
  Text ->
  IO FilePath
build srcDir exeTarget = do
  let imageName = "ghc-centos:" <> exeTarget

  buildDocker imageName
  -- build executable with docker
  printPending $ "building with srcDir: '" <> toS srcDir <> "' and exeTarget: '" <> exeTarget <> "' ..."
  exe <- stackInDocker (ImageName $ toS imageName) srcDir (SimpleTarget $ toS exeTarget)

  let buildDir = srcDir <> "/.build"
  -- ensure hidden build dir exists
  createDirectoryIfMissing True buildDir

  -- move and rename the exe to the .build dir
  let lbdHandlerPath = buildDir <> "/bootstrap"
  renameFile exe lbdHandlerPath
  setFileMode lbdHandlerPath executableByAll

  -- pack executable with js shim in .zip file
  let archivePath = buildDir <> "/lambda.zip"
  callProcess "zip" $ ["-j", archivePath, lbdHandlerPath]

  pure archivePath
  where
    buildDocker :: Text -> IO ()
    buildDocker imageName = callProcess "docker" ["build", "-t", toS imageName, "ghc-centos"]

    executableByAll :: FileMode
    executableByAll =
      foldl
        unionFileModes
        nullFileMode
        [ ownerModes,
          groupReadMode,
          groupExecuteMode,
          otherReadMode,
          otherExecuteMode
        ]

reconf ::
  forall x.
  AwsMode ->
  Service ->
  (AWS x -> AWS x)
reconf RealDeal _ = identity
reconf LocalStack svc = case _svcAbbrev svc of
  "API Gateway" -> setport 4567
  "CloudFormation" -> setport 4581
  "CloudWatch" -> setport 4582
  "DynamoDB" -> setport 4569
  "DynamoDB Streams" -> setport 4570
  "Elasticsearch" -> setport 4571
  "Firehose" -> setport 4573
  "Kinesis" -> setport 4568
  "Lambda" -> setport 4574
  "Redshift" -> setport 4577
  "Route53" -> setport 4580
  "S3" -> setport 4572
  "SES" -> setport 4579
  "Secrets Manager" -> setport 4584
  "SNS" -> setport 4575
  "SQS" -> setport 4576
  "SSM" -> setport 4583
  unknown -> panic $ show unknown
  where
    setport port = reconfigure (setEndpoint False "localhost" port svc)
