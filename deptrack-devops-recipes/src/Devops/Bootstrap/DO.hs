{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
-- | Provides bootstrapping nodes on Digital Ocean's cloud infrastructure
module Devops.Bootstrap.DO where

import           Data.Functor      (void)
import           Data.Maybe
import           Data.Monoid
import           Data.Text         (pack, unpack)
import           Devops.Base
import           Devops.Bootstrap
import           Devops.Networking
import           Devops.Ref
import           Network.DO

-- | A Digital Ocean node type
data DOcean = DOcean { doDropletConfig :: !BoxConfiguration }
        deriving (Show)

type instance NodeConfig DOcean = DOcean

deriving instance Show (Node DOcean)

instance HasResolver Remote (Node DOcean) where
  resolve k _ = runDOAuth $ do
    -- assume k is the name of the droplet to resolve
    droplets <- mapMaybe publicIP . findByIdOrName (unpack k) <$> listDroplets
    case droplets of
      (ip:_) -> return $ Remote (pack $ show ip)
      []      -> fail $ "cannot resolve Remote for droplet with name " <> unpack k


ubuntuXenialSlug :: ImageSlug
ubuntuXenialSlug =  "ubuntu-16-04-x64"

-- | A more-or-less standard droplet definition.
-- This `BoxConfiguration` has 1GB of RAM, is deployed on `Ams2` region and has *no* ssh keys.
-- it should probably be configured before being usable as a dependency.
standardDroplet :: BoxConfiguration
standardDroplet = BoxConfiguration "deptrack-default" (RegionSlug "ams2") G1 defaultImage [] False

-- | Describe a dependency on a DOcean droplet instance.
--
-- TODO: uniqueness of config?
droplet :: BoxConfiguration -> DevOp (Node DOcean)
droplet conf@BoxConfiguration{..} = devop id mkOp (pure $ Node $ DOcean conf)
  where
    mkOp _ = buildOp
      ("Digital Ocean droplet: " <> pack configName)
      ("creates DO droplet with configuration: " <> pack (show conf))
      (checkDropletExists configName)
      (startDroplet conf)
      (killDroplet configName)
      noAction

-- * DO Utilities

startDroplet :: BoxConfiguration -> OpAction
startDroplet conf = runDOAuth $ (createDroplet conf) >>= either (fail . show) (const $ return ())

checkDropletExists :: String -> OpCheck
checkDropletExists dname = runDOAuth $ withDroplet dname $
  maybe (return $ Failure $ "no droplet with name " <> dname) (const $ return Success)

killDroplet :: String -> OpAction
killDroplet dname = void $ runDOAuth $
  withDroplet dname $ maybe (return Nothing) (destroyDroplet . dropletId)

withDroplet :: String -> (Maybe Droplet -> Command IO a) -> Command IO a
withDroplet dropletName f = (findByIdOrName dropletName <$> listDroplets) >>= f . listToMaybe

runDOAuth :: Command IO a -> IO a
runDOAuth command = do
  auth <- getAuthFromEnv
  runDO command auth
