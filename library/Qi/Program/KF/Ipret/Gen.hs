module Qi.Program.KF.Ipret.Gen (run) where

import           Control.Lens           (Getting, (.~), (?~), (^.))
import           Data.Aeson             (encode)
import           Network.AWS.Firehose
import           Polysemy hiding (run)
import           Protolude              hiding ((<&>))

import           Qi.AWS.Resource
import           Qi.AWS.KF
import           Qi.AWS.Types
import           Qi.Config
import           Qi.Program.Config.Lang (ConfigEff, getConfig)
import           Qi.Program.Gen.Lang
import           Qi.Program.KF.Lang (KfEff (..))

run
  :: forall effs a
  .  (Member GenEff effs, Member ConfigEff effs)
  => (Sem (KfEff ': effs) a -> Sem effs a)
run = interpret (\case

  Put lid msg -> do
    Config{ _appName }  <- getConfig
    let pid = toPhysicalId _appName lid
    void . amazonka firehose $ putRecord
                                 (show pid)
                                 (record . toS $ encode msg)
                            -- & uS3Bucket ?~ show ( toPhysicalId _appName _s3oBucketId )

  )
