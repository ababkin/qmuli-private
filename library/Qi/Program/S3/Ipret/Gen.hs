{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}

module Qi.Program.S3.Ipret.Gen (run) where

import Control.Exception.Lens (handling)
import Control.Lens (Getting, (.~), (?~), (^.))
import qualified Data.Map.Strict as Map
import Network.AWS hiding (Request, Response, send)
import Network.AWS.Data.Body (RsBody (..))
import Network.AWS.Data.Text (ToText (..))
import Network.AWS.S3 hiding (bucket)
import Polysemy hiding (run)
import Protolude hiding ((<&>))
import Qi.AWS.S3
import Qi.AWS.Types
import Qi.Config
import Qi.Program.Config.Lang (ConfigEff, getConfig, s3Bucket)
import Qi.Program.Gen.Lang
import Qi.Program.S3.Internal (ListToken (ListToken))
import Qi.Program.S3.Lang (S3Eff (..))

run ::
  forall effs a.
  (Member GenEff effs, Member ConfigEff effs) =>
  (Sem (S3Eff ': effs) a -> Sem effs a)
run =
  interpret
    ( \case
        GetContent S3Object {_s3oBucketId, _s3oKey = S3Key (ObjectKey -> objKey)} ->
          action =<< getConfig
          where
            -- TODO: handle errors
            -- handling _KeyNotFound handler action

            action Config {_appName} =
              let bucketName = BucketName . show $ toPhysicalId _appName _s3oBucketId
               in amazonkaPostBodyExtract
                    s3
                    (getObject bucketName objKey)
                    (^. gorsBody)

        {-
                handler _ = pure $ Left "KeyNotFound"

                _KeyNotFound :: AsError a => Getting (First ServiceError) a ServiceError
                _KeyNotFound = _ServiceError . hasStatus 404 -- . hasCode "InvalidKeyPair.Duplicate"
        -}

        PutContent S3Object {_s3oBucketId, _s3oKey = S3Key (ObjectKey -> objKey)} payload -> do
          Config {_appName} <- getConfig
          let bucketName = BucketName . show $ toPhysicalId _appName _s3oBucketId
          void $ amazonka s3 $ putObject bucketName objKey (toBody payload) & poACL ?~ OPublicReadWrite
        ListObjects bucketId maybeToken -> do
          Config {_appName} <- getConfig
          let bucketName = BucketName . show $ toPhysicalId _appName bucketId
          r <- amazonka s3 $ case maybeToken of
            Nothing ->
              -- first pagination call
              listObjectsV2 bucketName
            Just (ListToken token) ->
              listObjectsV2 bucketName & lovContinuationToken ?~ token
          let objs = (\o -> S3Object bucketId $ S3Key . toText $ o ^. oKey) <$> (r ^. lovrsContents)

          pure $ (objs, ListToken <$> r ^. lovrsNextContinuationToken)
        DeleteObject S3Object {_s3oBucketId, _s3oKey = S3Key (ObjectKey -> objKey)} -> do
          Config {_appName} <- getConfig
          let bucketName = BucketName . show $ toPhysicalId _appName _s3oBucketId
          void $ amazonka s3 $ deleteObject bucketName objKey
        DeleteObjects s3objs -> do
          Config {_appName} <- getConfig
          let dict = Map.toList . Map.fromListWith (<>) $ toPair <$> s3objs

              toPair :: S3Object -> (BucketName, [ObjectIdentifier])
              toPair S3Object {_s3oBucketId, _s3oKey = S3Key (ObjectKey -> objKey)} =
                ( BucketName . show $ toPhysicalId _appName _s3oBucketId,
                  [objectIdentifier objKey]
                )

          for_ dict $ \(bucketName, objIds) ->
            amazonka s3 $ deleteObjects bucketName $ delete' & dObjects .~ objIds
    )
