{-# LANGUAGE TemplateHaskell     #-}

module Qi.Program.Gen.Lang where

-- import           Control.Monad.Freer
import qualified Control.Monad.Trans.AWS as AWS (send)
import           Data.Aeson              (FromJSON, ToJSON, Value)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import           Data.Time.Clock         (UTCTime)
import           Network.AWS             hiding (Request, Response, send)
import           Network.AWS.Data.Body   (RsBody (..))
import           Network.HTTP.Client
import           Protolude
import           Servant.Client          (BaseUrl, ClientM, ServantError)
import           Polysemy

import           Qi.AWS.Types


data GenEff m r where
  GetAppName
    :: GenEff m AppName

  Http
    :: ManagerSettings
    -> Request
    -> GenEff m (Response LBS.ByteString)

  RunServant
    :: ManagerSettings
    -> BaseUrl
    -> ClientM a
    -> GenEff m (Either ServantError a)

  Amazonka
    :: (AWSRequest a)
    => Service
    -> a
    -> GenEff m (Rs a)

  AmazonkaPostBodyExtract
    :: (AWSRequest a)
    => Service
    -> a
    -> (Rs a -> RsBody)
    -> GenEff m (Either Text LBS.ByteString)

  Say
    :: Text
    -> GenEff m ()

  GetCurrentTime
    :: GenEff m UTCTime

  Sleep
    :: Int
    -> GenEff m ()

  Build
    :: GenEff m Text -- TODO return status

  ReadFileLazy
    :: Text
    -> GenEff m LBS.ByteString

  PutStr
    :: LBS.ByteString
    -> GenEff m ()


makeSem ''GenEff

{-
getAppName
  :: (Member GenEff effs)
  => Eff effs AppName
getAppName =
  send GetAppName


http
  :: Member GenEff effs
  => ManagerSettings
  -> Request
  -> Eff effs (Response LBS.ByteString)
http =
  send .: Http


runServant
  :: Member GenEff effs
  => ManagerSettings
  -> BaseUrl
  -> ClientM a
  -> Eff effs (Either ServantError a)
runServant =
  send .:: RunServant


amazonka
  :: (AWSRequest a, Member GenEff effs)
  => Service
  -> a
  -> Eff effs (Rs a)
amazonka =
  send .: Amazonka


amazonkaPostBodyExtract
  :: (AWSRequest a, Member GenEff effs)
  => Service
  -> a
  -> (Rs a -> RsBody)
  -> Eff effs (Either Text LBS.ByteString)
amazonkaPostBodyExtract =
  send .:: AmazonkaPostBodyExtract


getCurrentTime
  :: Member GenEff effs
  => Eff effs UTCTime
getCurrentTime =
  send GetCurrentTime


sleep
  :: Member GenEff effs
  => Int
  -> Eff effs ()
sleep = send . Sleep


say
  :: Member GenEff effs
  => Text
  -> Eff effs ()
say = send . Say

build
  :: Member GenEff effs
  => Eff effs Text
build = send Build

readFileLazy
    :: Member GenEff effs
    => Text
    -> Eff effs LBS.ByteString
readFileLazy = send . ReadFileLazy

{- getReq -}
  {- :: Member GenEff effs -}
  {- => Eff effs BS.ByteString -}
{- getReq = send GetReq -}

putStr
  :: Member GenEff effs
  => LBS.ByteString
  -> Eff effs ()
putStr = send . PutStr


-}
