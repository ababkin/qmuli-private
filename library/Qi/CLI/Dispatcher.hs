{-# LANGUAGE DataKinds         #-}

module Qi.CLI.Dispatcher (withConfig) where

import           Data.Aeson                     (eitherDecode, encode)
import           Control.Lens
import           Control.Monad.Freer
import           Data.Aeson.Encode.Pretty       (encodePretty)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.Map                       as Map
import           Network.AWS.Data.Body          (toBody)
import           Network.AWS.S3
import           Protolude                      hiding (FilePath, all, State, runState)
import           Qi.CLI.Dispatcher.S3           as S3 (clearBuckets)
import           Qi.Program.CF.Lang
import           Qi.Program.Config.Lang         (getConfig)
import           Qi.Program.Gen.Lang
import           Qi.Program.Lambda.Lang         (LambdaEff)
import qualified Qi.Program.Lambda.Lang         as Lbd
import           Qi.Program.S3.Lang
import           Qi.Config.AWS
import           Qi.Config.AWS.Lambda           hiding (lbdName)
import qualified Qi.Config.AWS.Lambda.Accessors as Lbd
import           Qi.Config.AWS.S3
import qualified Qi.Config.AWS.S3.Event         as S3Event
import qualified Qi.Config.CfTemplate           as CF
import           Qi.Config.Types                (ResourceExistence (AlreadyExists))
import           Qi.CLI.Options
import           Qi.AWS.Types
import           Qi.AWS.Resource
import qualified Qi.Program.Config.Ipret.State  as Config
import           Qi.Program.Config.Lang         (ConfigEff, s3Bucket)
import qualified Qi.Program.Gen.Lang            as Gen
import qualified Qi.Program.Wiring.IO           as IO
import           Qi.AWS.Logger (mkLambdaLogger, mkCliLogger)
import           Qi.AWS.Runtime  (Response(SuccessResponse, ErrorResponse), ErrorCode(ErrorCode), getEndpoint, getWithRetries, HandlerRequest(..), respond, HandlerResponse(..))
import           Qi.AWS.Types (AwsMode(RealDeal))
import           System.Environment             (lookupEnv)
import           System.IO                      (BufferMode (..), hSetBuffering,
                                                 stderr, stdout)
import           Data.Default                   (def)
import           Data.Text                      (splitOn)
import           Control.Monad.Freer.State
import           Data.Aeson.Types               (parseEither)


withConfig
  :: Eff '[ConfigEff, State Config] ()
  -> IO ()
withConfig configProgram = do
  -- `showHelpOnErrorExecParser` parses out commands, arguments and options using the rules in `opts`
  -- and gives the `Options` structure to `dispatch` that acts in accord to the options
  opts <- showHelpOnErrorExecParser optionsSpec

  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  let mkConfig appName =
          snd
        . run
        . runState def{_namePrefix = appName}
        . Config.run
        $ do
          s3Bucket "app" $ def & s3bpExistence .~ AlreadyExists -- always assume existence of the app bucket
          configProgram

  case opts of
    LbdDispatch -> do
      let splitNames compositeName = case splitOn "_" compositeName of
                        (h:t) -> (h, mconcat t)
                        _ -> panic "could not split the app and lbd names from the composite name"

      (appName, lbdName) <- maybe
                (panic "AWS_LAMBDA_FUNCTION_NAME not found in ENV")
                (splitNames . toS)
              <$> lookupEnv "AWS_LAMBDA_FUNCTION_NAME"

      let config = mkConfig appName
          runLambda = IO.run config RealDeal mkLambdaLogger
      either panic (loop config lbdName runLambda) =<< getEndpoint


    Management ManagementOptions{ appName, cmd, awsMode } -> do
      let config = mkConfig appName
          runCli = IO.run config awsMode mkCliLogger
          template = CF.render config

      runCli $ case cmd of
        CfRenderTemplate     -> Gen.say $ toS template
        CfDeploy             ->
                Gen.build
            >>= Gen.readFileLazy
            >>= deployApp template

        CfCreate             -> createCfStack template
        CfUpdate             -> updateCfStack template
        CfDescribe           -> describeCfStack
        CfDestroy            -> destroyCfStack $ pure ()

        CfCycle              ->
                Gen.build
            >>= Gen.readFileLazy
            >>= cycleStack template

        LbdUpdate -> updateLambdas


  where
    -- this loops over accumulated lambda calls and handles them
    loop config lbdName runLambda endpoint = loop'
      where
        loop' = do
          req' <- getWithRetries 3 endpoint
          case req' of
            SuccessResponse HandlerRequest{ payload, requestId } -> do
              resp <- runLambda $ lbdHandler config lbdName payload
              respond endpoint requestId $ SuccessHandlerResponse (toS resp) (Just "application/json")
              loop'

            ErrorResponse code ->
              case code of
                ErrorCode (-1) ->
                  panic ("Failed to send HTTP request to retrieve next task." :: Text)
                _ -> do
                  print ("HTTP request was not successful. HTTP response code: " <>
                     show code <>
                     ". Retrying.." :: Text)
                  loop'


    lbdHandler config name req =
          let id = LogicalId name
              reportBadArgument lbdType err =
                panic $ "Could not parse event: '" <> toS req <>
                  "', for lambda type: '" <> lbdType <> "' error was: '" <> toS err <> "'"
          in
          case getById config id of

            GenericLambda{ _lbdGenericLambdaProgram } ->
              either  (reportBadArgument "Generic")
                      (map encode . _lbdGenericLambdaProgram)
                      $ eitherDecode (toS req)

            S3BucketLambda{ _lbdS3BucketLambdaProgram } ->
              either  (reportBadArgument "S3")
                      _lbdS3BucketLambdaProgram
                      $ parseEither (S3Event.parse config) =<< eitherDecode (toS req)


deployApp
  :: Members '[ S3Eff, GenEff, ConfigEff ] effs
  => LBS.ByteString
  -> LBS.ByteString
  -> Eff effs ()
deployApp _template content = do
  say "deploying the app..."
  config <- getConfig
  let appName     = config ^. namePrefix
      bucketName = BucketName $ appName <> ".app"

  say $ "creating bucket '" <> show bucketName <> "'"
  amazonka s3 $ createBucket bucketName

  say $ "writing lambda executable into bucket '" <> show bucketName <> "'"
  amazonka s3 $ putObject bucketName "lambda.zip" (toBody content) & poACL ?~ OPublicReadWrite
  pass

createCfStack
  :: Members '[ CfEff, GenEff, ConfigEff ] effs
  => LBS.ByteString
  -> Eff effs ()
createCfStack template = do
  config <- getConfig
  let appName     = config ^. namePrefix
      stackName   = StackName appName

  say "creating the stack..."
  createStack stackName template

  say "waiting on the stack to be created..."
  waitOnStackStatus stackName SSCreateComplete NoAbsent

  say "stack was successfully created"


updateCfStack
  :: Members '[ LambdaEff, CfEff, GenEff, ConfigEff ] effs
  => LBS.ByteString
  -> Eff effs ()
updateCfStack template = do
  config <- getConfig
  let appName     = config ^. namePrefix
      stackName   = StackName appName

  say "updating the stack..."
  updateStack stackName template

  say "waiting on the stack to be updated..."
  waitOnStackStatus stackName SSUpdateComplete NoAbsent

  -- TODO: make lambda updating concurrent with the above stack update?
  updateLambdas

  say "stack was successfully updated"


updateLambdas
  :: Members '[ LambdaEff, GenEff, ConfigEff ] effs
  => Eff effs ()
updateLambdas = do
  config <- getConfig
  let lbdS3Obj = S3Object (LogicalId "app") $ S3Key "lambda.zip"

  say "updating the lambdas..."
  traverse_ ((`Lbd.update` lbdS3Obj) . logicalId config) (all config :: [ Lambda ])


describeCfStack
  :: Members '[ CfEff, GenEff, ConfigEff ] effs
  => Eff effs ()
describeCfStack = do
  config <- getConfig
  let stackName = StackName $ config ^. namePrefix
  stackDict <- describeStacks
  maybe (panic $ "stack '" <> show stackName <> "' not found") (say . toS . encodePretty) $ Map.lookup stackName stackDict


destroyCfStack
  :: Members '[ LambdaEff, CfEff, S3Eff, GenEff, ConfigEff ] effs
  => Eff effs ()
  -> Eff effs ()
destroyCfStack action = do
  config <- getConfig
  let stackName = StackName $ config ^. namePrefix

  say "destroying the stack..."

  clearBuckets
  deleteStack stackName

  action

  say "waiting on the stack to be destroyed..."
  waitOnStackStatus stackName SSDeleteComplete AbsentOk

  say "stack was successfully destroyed"


cycleStack
  :: Members '[ LambdaEff, CfEff, S3Eff, GenEff, ConfigEff ] effs
  => LBS.ByteString
  -> LBS.ByteString
  -> Eff effs ()
cycleStack template content = do
  destroyCfStack $ deployApp template content
  createCfStack template
  say "all done!"


