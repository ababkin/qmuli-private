module Qi.Program.KF.Ipret.Gen where

import           Control.Lens           (Getting, (.~), (?~), (^.))
import           Control.Monad.Freer
import           Data.Aeson             (encode)
import           Protolude              hiding ((<&>))
import           Qi.AWS.Resource
import           Qi.AWS.KF
import           Qi.AWS.Types
import           Qi.Config
import           Qi.Program.Config.Lang (ConfigEff, getConfig)
import           Qi.Program.Gen.Lang
import           Qi.Program.KF.Lang (KfEff (..))
import           Network.AWS.Firehose

run
  :: forall effs a
  .  Members [GenEff, ConfigEff] effs
  => (Eff (KfEff ': effs) a -> Eff effs a)
run = interpret (\case

  Put lid msg -> do
    Config{ _appName }  <- getConfig
    let pid = toPhysicalId _appName lid
    void . amazonka firehose $ putRecord
                                 (show pid)
                                 (record . toS $ encode msg)
                            -- & uS3Bucket ?~ show ( toPhysicalId _appName _s3oBucketId )

  )
