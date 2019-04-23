{-# LANGUAGE TemplateHaskell     #-}

module Qi.Program.Lambda.Lang where

import           Data.Aeson          (ToJSON)
import           Protolude
import           Polysemy

import           Qi.AWS.S3           (S3Object)
import           Qi.AWS.Types


type LambdaFunctionId = LogicalId 'LambdaFunctionResource

data LambdaEff m r where

  Invoke
    :: ToJSON a
    => LambdaFunctionId
    -> a
    -> LambdaEff m ()

  Update
    :: LambdaFunctionId
    -> S3Object
    -> LambdaEff m ()

makeSem ''LambdaEff
