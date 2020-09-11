{-# LANGUAGE TemplateHaskell #-}

module Qi.Program.Gen.Lang where

import qualified Control.Monad.Trans.AWS as AWS (send)
import Data.Aeson (FromJSON, ToJSON, Value)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (UTCTime)
import Network.AWS hiding (Request, Response, send)
import Network.AWS.Data.Body (RsBody (..))
import Network.HTTP.Client
import Polysemy
import Protolude
import Qi.AWS.Types (AppName)
import Servant.Client (BaseUrl, ClientM)
import Servant.Client.Core.ClientError (ClientError)

data GenEff m r where
  Http ::
    ManagerSettings ->
    Request ->
    GenEff m (Response LBS.ByteString)
  RunServant ::
    ManagerSettings ->
    BaseUrl ->
    ClientM a ->
    GenEff m (Either ClientError a)
  Amazonka ::
    (AWSRequest a) =>
    Service ->
    a ->
    GenEff m (Rs a)
  AmazonkaPostBodyExtract ::
    (AWSRequest a) =>
    Service ->
    a ->
    (Rs a -> RsBody) ->
    GenEff m (Either Text LBS.ByteString)
  Say ::
    Text ->
    GenEff m ()
  GetCurrentTime ::
    GenEff m UTCTime
  Sleep ::
    Int ->
    GenEff m ()
  Build ::
    GenEff m Text -- TODO return status
  ReadFileLazy ::
    Text ->
    GenEff m LBS.ByteString
  PutStr ::
    LBS.ByteString ->
    GenEff m ()

makeSem ''GenEff
