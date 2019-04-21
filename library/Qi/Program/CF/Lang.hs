{-# LANGUAGE TemplateHaskell     #-}

module Qi.Program.CF.Lang (
    CfEff (..)
  , createStack
  , describeStacks
  , updateStack
  , deleteStack
  , waitOnStackStatus
  , StackStatus(..)
  , AbsentDirective(..)
  , StackDescription(..)
  , StackDescriptionDict
  )where


import           Data.Aeson                 hiding ((.:))
import qualified Data.ByteString.Lazy       as LBS
import           Data.HashMap.Strict        (fromList)
import           Data.Map                   (Map)
import           Data.Composition
import           Network.AWS.CloudFormation (StackStatus (..))
import           Network.AWS.S3.Types       (ETag)
import           Protolude
import           Polysemy

import           Qi.AWS.S3
import           Qi.AWS.Types
import           Qi.Program.Gen.Lang


type StackDescriptionDict = Map AppName StackDescription

data StackDescription = StackDescription {
    status  :: StackStatus
  , outputs :: [(Text, Text)]
  } deriving (Show)

instance ToJSON StackDescription where
  toJSON StackDescription{ status, outputs } =
    object  [ "status"  .= String (show status :: Text)
            , "outputs" .= (Object $ fromList $ second String <$> outputs)
            ]


data AbsentDirective = AbsentOk | NoAbsent
  deriving Eq

data CfEff m r where

  CreateStack
    :: AppName
    -> LBS.ByteString
    -> CfEff m ()

  DescribeStacks
    :: CfEff m StackDescriptionDict

  UpdateStack
    :: AppName
    -> LBS.ByteString
    -> CfEff m ()

  DeleteStack
    :: AppName
    -> CfEff m ()

  WaitOnStackStatus
    :: AppName
    -> StackStatus
    -> AbsentDirective
    -> CfEff m ()

makeSem ''CfEff

-- createStack
--   :: (Member CfEff effs)
--   => AppName
--   -> LBS.ByteString -- S3Object
--   -> Eff effs ()
-- createStack = send .: CreateStack

-- describeStacks
--   :: (Member CfEff effs)
--   => Eff effs StackDescriptionDict
-- describeStacks = send DescribeStacks

-- updateStack
--   :: (Member CfEff effs)
--   => AppName
--   -> LBS.ByteString
--   -> Eff effs ()
-- updateStack = send .: UpdateStack

-- deleteStack
--   :: (Member CfEff effs)
--   => AppName
--   -> Eff effs ()
-- deleteStack = send . DeleteStack

-- waitOnStackStatus
--   :: (Member CfEff effs)
--   => AppName
--   -> StackStatus
--   -> AbsentDirective
--   -> Eff effs ()
-- waitOnStackStatus = send .:: WaitOnStackStatus
