{-# LANGUAGE TemplateHaskell #-}

module Qi.Program.S3.Lang
  ( S3Eff (..),
    ListToken,
    getContent,
    putContent,
    listObjects,
    deleteObject,
    deleteObjects,
    S3LambdaProgram,
  )
where

import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as LBS
import Network.AWS.S3.Types (ETag)
import Polysemy
import Protolude
import Qi.AWS.S3
import Qi.AWS.Types
import Qi.Program.Gen.Lang
import Qi.Program.S3.Internal (ListToken)

type S3LambdaProgram effs = S3Event -> Sem effs Value

data S3Eff m r where
  GetContent ::
    S3Object ->
    S3Eff m (Either Text LBS.ByteString)
  PutContent ::
    S3Object ->
    LBS.ByteString ->
    S3Eff m ()
  ListObjects ::
    S3BucketId ->
    Maybe ListToken ->
    S3Eff m ([S3Object], Maybe ListToken)
  DeleteObject ::
    S3Object ->
    S3Eff m ()
  DeleteObjects ::
    [S3Object] ->
    S3Eff m ()

{-
  MultipartS3Upload
    :: S3Object
    -> (S3Object -> Text -> LambdaProgram [(Int, ETag)])
    -> S3Eff ()

  UploadS3Chunk
    :: S3Object -- sink
    -> Text -- uploadId
    -> (Int, S3Object) -- source chunk
    -> S3Eff (Maybe (Int, ETag))
-}

makeSem ''S3Eff
