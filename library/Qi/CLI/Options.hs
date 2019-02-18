module Qi.CLI.Options where

import           Options.Applicative
import           Protolude           hiding (runState)
import           Qi.AWS.Types


data Options = LbdDispatch | Management ManagementOptions

data ManagementOptions = ManagementOptions {
    cmd     :: Command
  , appName :: AppName
  , awsMode :: AwsMode
  }

data Command =
    CfRenderTemplate
  | CfDeploy
  | CfCreate
  | CfUpdate
  | CfDescribe
  | CfDestroy
  | CfCycle
  | LbdUpdate
  -- | LbdLogs Text


optionsSpec :: ParserInfo Options
optionsSpec = info (helper <*> (managementOptionsParser <|> pure LbdDispatch)) fullDesc

managementOptionsParser :: Parser Options
managementOptionsParser = map Management $ ManagementOptions
  <$> hsubparser (cfCmd <> lbdCmd)
  <*> appNameParser
  <*> awsModeOption


appNameParser :: Parser AppName
appNameParser =
  either panic identity . mkAppName <$> textArg "APP_NAME"

-- | Creates a Text positional argument with the given name
textArg
  :: Text
  -> Parser Text
textArg = argument str . metavar . toS


cfCmd :: Mod CommandFields Command
cfCmd =
    command "cf"
  $ info cfParser
  $ fullDesc <> progDesc "Perform operations on CloudFormation stack"
  where
    cfParser = hsubparser (   cfCreate
                          <>  cfDeploy
                          <>  cfDescribe
                          <>  cfUpdate
                          <>  cfDestroy
                          <>  cfCycle
                          <>  cfTemplate
                          )

    cfCreate :: Mod CommandFields Command
    cfCreate =
        command "create"
      $ info (pure CfCreate)
      $ fullDesc <> progDesc "Create a CloudFormation stack"

    cfDeploy :: Mod CommandFields Command
    cfDeploy =
        command "deploy"
      $ info (pure CfDeploy)
      $ fullDesc <> progDesc "Deploy a CloudFormation stack"

    cfDescribe :: Mod CommandFields Command
    cfDescribe =
        command "describe"
      $ info (pure CfDescribe)
      $ fullDesc <> progDesc "Describe a CloudFormation stack"

    cfUpdate :: Mod CommandFields Command
    cfUpdate =
        command "update"
      $ info (pure CfUpdate)
      $ fullDesc <> progDesc "Update a CloudFormation stack"

    cfDestroy :: Mod CommandFields Command
    cfDestroy =
        command "destroy"
      $ info (pure CfDestroy)
      $ fullDesc <> progDesc "Destroy a CloudFormation stack"

    cfCycle :: Mod CommandFields Command
    cfCycle =
        command "cycle"
      $ info (pure CfCycle)
      $ fullDesc <> progDesc "Destroy the CloudFormation stack, re-deploy the app then re-create a stack"


    cfTemplate :: Mod CommandFields Command
    cfTemplate =
        command "render"
      $ info (pure CfRenderTemplate)
      $ fullDesc <> progDesc "Renders the CloudFormation template"





lbdCmd :: Mod CommandFields Command
lbdCmd =
    command "lbd"
  $ info lbdUpdateParser
  $ fullDesc <> progDesc "Perform Lambda operations"
  where
    lbdUpdateParser = hsubparser lbdUpdate

    lbdUpdate :: Mod CommandFields Command
    lbdUpdate =
        command "update"
      $ info (pure LbdUpdate)
      $ fullDesc <> progDesc "Update Lambda"


lambdaNameOption :: Parser Text
lambdaNameOption = strOption $
  long "lambda-name"
    <> metavar "LAMBDA_NAME"
    <> help "Name of the Lambda function to call"

awsModeOption :: Parser AwsMode
awsModeOption = flag RealDeal LocalStack $
  long "local-stack"
    <> help "Specify whether to use localstack mock or the real AWS"

-- | A version of 'execParser' which shows full help on error.
--
-- The regular 'execParser' only prints usage on error, which doesn't
-- include the options, subcommands, or mention of the help switch
-- @--help@.
showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)



