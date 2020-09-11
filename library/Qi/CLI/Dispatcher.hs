{-# LANGUAGE ConstraintKinds #-}

module Qi.CLI.Dispatcher (withConfig) where

import Control.Lens
import Data.Aeson (eitherDecode, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import Data.Default (def)
import qualified Data.Map as Map
import Data.Text (splitOn)
import Network.AWS.Data.Body (toBody)
import Network.AWS.S3
import Polysemy
import Polysemy.State
import Protolude hiding (FilePath, State, all, runState)
import Qi.AWS.Lambda.Function
import Qi.AWS.Logger (mkCliLogger, mkLambdaLogger)
import qualified Qi.AWS.Render as R
import Qi.AWS.Runtime (ErrorCode (ErrorCode), HandlerRequest (..), HandlerResponse (..), Response (ErrorResponse, SuccessResponse), getEndpoint, getWithRetries, respond)
import Qi.AWS.S3
import Qi.AWS.Types
import Qi.AWS.Types (AwsMode (RealDeal))
import Qi.CLI.Dispatcher.S3 as S3 (clearBuckets)
import Qi.CLI.Options
import Qi.Config hiding (appName)
import Qi.Program.CF.Lang
import qualified Qi.Program.Config.Ipret.State as Config
import Qi.Program.Config.Lang (ConfigEff, getConfig, s3Bucket)
import Qi.Program.Gen.Lang
import qualified Qi.Program.Gen.Lang as Gen
import Qi.Program.Lambda.Lang (LambdaEff)
import qualified Qi.Program.Lambda.Lang as Lbd
import Qi.Program.S3.Lang
import qualified Qi.Program.Wiring.IO as IO
import System.Environment (lookupEnv)
import System.IO
  ( BufferMode (..),
    hSetBuffering,
    stderr,
    stdout,
  )

withConfig ::
  Sem '[ConfigEff, State Config] () ->
  IO ()
withConfig configProgram = do
  -- `showHelpOnErrorExecParser` parses out commands, arguments and options using the rules in `opts`
  -- and gives the `Options` structure to `dispatch` that acts in accord to the options
  opts <- showHelpOnErrorExecParser optionsSpec

  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  let runConfig appName =
        fst
          . run
          . runState (mkConfig appName)
          . Config.run
          $ do
            s3Bucket "app" $ def & s3bpExistence .~ AlreadyExists -- always assume existence of the app bucket
            configProgram

  case opts of
    LbdDispatch -> do
      lambdaId <-
        maybe
          (panic "AWS_LAMBDA_FUNCTION_NAME not found in ENV")
          toS
          <$> lookupEnv "AWS_LAMBDA_FUNCTION_NAME"
      case parsePhysicalId lambdaId of
        Left err -> panic err
        Right pid -> do
          let appName = toAppName pid
              lambdaLogicalId = toLogicalId pid
              config = runConfig appName
          either panic (loop config lambdaLogicalId) =<< getEndpoint
    Management ManagementOptions {appName, cmd, awsMode} -> do
      let config = runConfig appName
          runCli = IO.run config awsMode mkCliLogger
          template = R.render config

      runCli $ case cmd of
        CfRenderTemplate -> Gen.say $ toS template
        CfDeploy ->
          Gen.build
            >>= Gen.readFileLazy
            >>= deployApp template
        CfCreate -> createCfStack template
        CfUpdate -> updateCfStack template
        CfDescribe -> describeCfStack
        CfDestroy -> destroyCfStack $ pure ()
        CfCycle ->
          Gen.build
            >>= Gen.readFileLazy
            >>= cycleStack template
        LbdUpdate -> updateLambdas
  where
    -- this loops over accumulated lambda calls and handles them
    loop config lid endpoint = loop'
      where
        loop' = do
          req' <- getWithRetries 3 endpoint
          case req' of
            SuccessResponse HandlerRequest {payload, requestId} -> do
              resp <- IO.run config RealDeal mkLambdaLogger $ lbdHandler payload
              respond endpoint requestId $ SuccessHandlerResponse (toS resp) (Just "application/json")
              loop'
            ErrorResponse code ->
              case code of
                ErrorCode (-1) ->
                  panic ("Failed to send HTTP request to retrieve next task." :: Text)
                _ -> do
                  print
                    ( "HTTP request was not successful. HTTP response code: "
                        <> show code
                        <> ". Retrying.." ::
                        Text
                    )
                  loop'

        lbdHandler req =
          let reportBadArgument lbdType err =
                panic $
                  "Could not parse event: '" <> toS req
                    <> "', for lambda type: '"
                    <> lbdType
                    <> "' error was: '"
                    <> toS err
                    <> "'"
           in -- NOTE: not sure how to improve this, seems to require pattern-matching
              case getById config lid of
                LambdaFunction {program} ->
                  either
                    (reportBadArgument "Lambda")
                    (map encode . program)
                    $ eitherDecode (toS req)

type Basic effs = (Member GenEff effs, Member ConfigEff effs)

deployApp ::
  (Member S3Eff effs, Basic effs) =>
  LBS.ByteString ->
  LBS.ByteString ->
  Sem effs ()
deployApp _template content = do
  say "deploying the app..."
  Config {_appName} <- getConfig
  let bucketName = BucketName $ show _appName <> ".app"

  say $ "creating bucket '" <> show bucketName <> "'"
  amazonka s3 $ createBucket bucketName

  say $ "writing lambda executable into bucket '" <> show bucketName <> "'"
  amazonka s3 $ putObject bucketName "lambda.zip" (toBody content) & poACL ?~ OPublicReadWrite
  pass

createCfStack ::
  (Member CfEff effs, Basic effs) =>
  LBS.ByteString ->
  Sem effs ()
createCfStack template = do
  Config {_appName} <- getConfig

  say "creating the stack..."
  createStack _appName template

  say "waiting on the stack to be created..."
  waitOnStackStatus _appName SSCreateComplete NoAbsent

  say "stack was successfully created"

updateCfStack ::
  (Member CfEff effs, Member LambdaEff effs, Basic effs) =>
  LBS.ByteString ->
  Sem effs ()
updateCfStack template = do
  Config {_appName} <- getConfig

  say "updating the stack..."
  updateStack _appName template

  say "waiting on the stack to be updated..."
  waitOnStackStatus _appName SSUpdateComplete NoAbsent

  -- TODO: make lambda updating concurrent with the above stack update?
  updateLambdas

  say "stack was successfully updated"

updateLambdas ::
  (Member LambdaEff effs, Basic effs) =>
  Sem effs ()
updateLambdas = do
  config <- getConfig
  let lbdS3Obj = S3Object (either (panic "unexpected") identity $ mkLogicalId "app") $ S3Key "lambda.zip"

  say "updating the lambdas..."
  traverse_ ((`Lbd.update` lbdS3Obj) . fst) (all config :: [(LambdaId, LambdaFunction)])

describeCfStack ::
  (Member CfEff effs, Basic effs) =>
  Sem effs ()
describeCfStack = do
  Config {_appName} <- getConfig
  stackDict <- describeStacks
  maybe (panic $ "stack '" <> show _appName <> "' not found") (say . toS . encodePretty) $ Map.lookup _appName stackDict

destroyCfStack ::
  (Member S3Eff effs, Member CfEff effs, Member LambdaEff effs, Basic effs) =>
  Sem effs () ->
  Sem effs ()
destroyCfStack action = do
  Config {_appName} <- getConfig
  say "destroying the stack..."
  clearBuckets
  deleteStack _appName
  action
  say "waiting on the stack to be destroyed..."
  waitOnStackStatus _appName SSDeleteComplete AbsentOk
  say "stack was successfully destroyed"

cycleStack ::
  (Member S3Eff effs, Member CfEff effs, Member LambdaEff effs, Basic effs) =>
  LBS.ByteString ->
  LBS.ByteString ->
  Sem effs ()
cycleStack template content = do
  destroyCfStack $ deployApp template content
  createCfStack template
  say "all done!"
