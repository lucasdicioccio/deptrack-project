{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | A module to run static-sites that are fetched with Git and served with
-- nginx.
module Devops.StaticSite (
    reverseProxiedStaticSiteConfig
  , reverseProxy
  ) where

import           Control.Distributed.Closure    (cap, cmap, cpure)
import           Control.Distributed.Closure.TH
import qualified Data.Text                      as Text
import           System.FilePath.Posix          (takeFileName, (</>))

import           Devops.Debian.Commands
import           Devops.Debian.User
import           Devops.Git
import           Devops.Networking
import           Devops.Nginx
import           Devops.Parasite
import           Devops.Service
import           Devops.Storage
import           Devops.Base

-- | Clones static sites in a homedir for a user named 'staticsites' and starts nginx.
staticSites :: RunDir -> [(HostString,GitUrl,GitBranch)] -> DevOp (Listening WebService)
staticSites rundir infos = do
  let dir hostdir = userDirectory (Text.unpack hostdir) (mereUser "staticsites")
  let repo (hostname,url,branch) = gitClone url branch git (dir hostname)
  let configs = map (nginxConfigFromClonedRepo . repo) infos
  let srv = nginxMainServer rundir configs
  (fmap . fmap) (const WebService) srv

nginxConfigFromClonedRepo :: DevOp GitRepo -> DevOp NginxServerConfig
nginxConfigFromClonedRepo mkRepo = do
  let locs = [ location "/" staticSiteDirectives ]
  (GitRepo dir@(DirectoryPresent dirname) _ _) <- mkRepo
  return $ server 80 (Text.pack $ takeFileName dirname) dir "index.html" locs

-- | Prepares nginx setup to run in proxy-pass mode.
-- Static sites will be started in an upstream (parasited) host and be cloned
-- in the homedir of 'staticsites' user on the parasite.
reverseProxiedStaticSiteConfig ::
     RunDir
  -> [(HostString,GitUrl,GitBranch)]
  -> DevOp (ParasitedHost)
  -> [DevOp NginxServerConfig]
reverseProxiedStaticSiteConfig rundir infos host =
  let staticSiteClosure = cap (cmap (static staticSites) (cpure $cdict rundir)) (cpure $cdict infos)
      http = (fmap . fmap . fmap) (const WebService) (remoted staticSiteClosure host)
      config (hostname,_,_) = nginxProxyPassConfigFromService rundir hostname http
      configs = map config infos
  in configs

nginxProxyPassConfigFromService :: RunDir
                                -> HostString
                                -> DevOp (Remoted (Listening WebService))
                                -> DevOp NginxServerConfig
nginxProxyPassConfigFromService rundir hostname mkHttp = do
  (Remoted (Remote ip) (Listening port _)) <- mkHttp
  dir <- directory (rundir </> "www")
  let locs = [ location "/" (proxyPassDirectives ip port) ]
  return $ server 80 hostname dir "index.html" locs

-- | Exposes a nginx server given sites configurations.
reverseProxy :: RunDir
             -> [DevOp NginxServerConfig]
             -> DevOp (Exposed (Daemon Nginx))
reverseProxy rundir configs = do
  let srv = nginxMainServer rundir configs
  publicFacingService srv
