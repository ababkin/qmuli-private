{-# LANGUAGE OverloadedLists #-}

module Qi.Program.CF.Ipret.Gen (run) where

import qualified Data.ByteString.Lazy as BS
import Data.Text.Encoding (decodeUtf8Lenient)
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Amazonka.CloudFormation.Types.Stack as Stack
import qualified Amazonka.CloudFormation.DescribeStacks as SOps
import qualified Amazonka.CloudFormation.CreateStack as SOps
import qualified Amazonka.CloudFormation.UpdateStack as SOps
import qualified Amazonka.CloudFormation.DeleteStack as SOps
import qualified Amazonka.CloudFormation.Types as Az
import qualified Amazonka.CloudFormation as Az
-- import Network.AWS.CloudFormation
  -- https://github.com/brendanhay/amazonka/blob/main/lib/services/amazonka-cloudformation/gen/Amazonka/CloudFormation/Types/Capability.hs
  -- ( Capability (..),
  -- https://github.com/brendanhay/amazonka/blob/main/lib/services/amazonka-cloudformation/gen/Amazonka/CloudFormation/Types/StackStatus.hs
  --   StackStatus (..),
  --   cloudFormation,
  --   createStack,
  --   csCapabilities,
  --   csTemplateBody,
  --   dStackName,
  --   deleteStack,
  --   describeStacks,
  --   dsRetainResources,
  --   dsrsStacks,
  --   lsrsStackSummaries,
  --   oOutputKey,
  --   oOutputValue,
  --   sOutputs,
  --   sStackName,
  --   sStackStatus,
  --   ssStackName,
  --   ssStackStatus,
  --   updateStack,
  --   usCapabilities,
  --   usTemplateBody,
  -- )

-- import Network.AWS.S3
--   ( BucketName (BucketName),
--     ObjectKey (ObjectKey),
--   )
import Polysemy hiding (run)
import Protolude hiding ((<&>))
import Qi.AWS.S3
import Qi.AWS.Types
import Qi.Config
import Qi.Program.CF.Lang
  ( AbsentDirective (..),
    CfEff (..),
    StackDescription (..),
    StackDescriptionDict,
  )
import Qi.Program.Config.Lang (ConfigEff, getConfig)
import Qi.Program.Gen.Lang

run ::
  forall effs a.
  (Member GenEff effs, Member ConfigEff effs) =>
  (Sem (CfEff ': effs) a -> Sem effs a)
run =
  interpret
    ( \case
        CreateStack name template -> do
          void . amazonka $
            SOps.newCreateStack (show name)
              & SOps.createStack_templateBody ?~ (decodeUtf8Lenient $ BS.toStrict template)
              & SOps.createStack_capabilities .~ Just [Az.Capability_CAPABILITY_NAMED_IAM]
        UpdateStack name template -> do
          void . amazonka $
            SOps.newUpdateStack (show name)
              & SOps.updateStack_templateBody ?~ (decodeUtf8Lenient $ BS.toStrict template)
              & SOps.updateStack_capabilities .~ Just [Az.Capability_CAPABILITY_NAMED_IAM]
        DeleteStack name ->
          void . amazonka $
            SOps.newDeleteStack (show name)
              & SOps.deleteStack_retainResources .~ Nothing
        DescribeStacks ->
          getStackDescriptions
        WaitOnStackStatus name status' isAbsentOk -> do
          let loop = sleep 1000000 >> go
              go = do
                stackDict <- getStackDescriptions
                case Map.lookup name stackDict of
                  Just StackDescription {status} | status == status' -> pure ()
                  Just _ -> loop -- wait for the stack state to change
                  Nothing -> case isAbsentOk of -- no mention of the stack in the log
                    AbsentOk -> pure () -- it's fine, don't wait any longer
                    NoAbsent -> loop -- keep waiting for the stack to appear in the log
          go
    )
  where
    getStackDescriptions :: Sem effs StackDescriptionDict
    getStackDescriptions = do
      r <- amazonka SOps.newDescribeStacks
      -- & dStackName ?~ name
      pure . Map.fromList $
        ( \stack ->
            ( either
                (panic "AWS returned an incorrectly looking app name")
                identity
                . mkAppName
                $ Stack.stackName stack,
              StackDescription
                { status = Stack.stackStatus stack,
                  outputs =
                    catMaybes $
                      ( \o -> do
                          key <- Az.outputKey o
                          val <- Az.outputValue o
                          pure (key, val)
                      )
                        <$> collapseMaybe (stack ^. Az.stack_outputs)
                }
            )
        )
          <$> collapseMaybe (r ^. SOps.describeStacksResponse_stacks)

    collapseMaybe :: forall i . Maybe [i] -> [i]
    collapseMaybe Nothing = []
    collapseMaybe (Just x) = x
{-

updateStack
  :: Text
  -> AWS ()
updateStack name =
  void . send $ CF.updateStack name
            & usTemplateURL ?~ T.concat ["https://s3.amazonaws.com/", name, "/cf.json"]
            & usCapabilities .~ [CapabilityNamedIAM]

deleteStack
  :: Text
  -> AWS ()
deleteStack name =
  void . send $ CF.deleteStack name
                  & dsRetainResources .~ []

describeStack
 :: Text
 -> AWS StackDescription
describeStack name = do
  r <- send $ CF.describeStacks
                & dStackName ?~ name
  case listToMaybe $ r ^ .dsrsStacks of
    Just stack ->
      return $ StackDescription {
          sdStatus = T.pack . show $ stack^.sStackStatus
        , sdOutputs = map (\o -> (fromJust $ o^.oOutputKey, fromJust $ o^.oOutputValue)) $ stack^.sOutputs
        }
    Nothing ->
      panic "Error: no stack description was returned"

- -}
