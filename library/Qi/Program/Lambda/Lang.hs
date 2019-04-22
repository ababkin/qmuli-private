{-# LANGUAGE TemplateHaskell     #-}

module Qi.Program.Lambda.Lang where

import           Data.Aeson          (ToJSON)
import           Protolude
import           Polysemy

import           Qi.AWS.S3           (S3Object)
import           Qi.AWS.Types


type LambdaId = LogicalId 'LambdaResource

data LambdaEff m r where

  Invoke
    :: ToJSON a
    => LambdaId
    -> a
    -> LambdaEff m ()

  Update
    :: LambdaId
    -> S3Object
    -> LambdaEff m ()

makeSem ''LambdaEff
