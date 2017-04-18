{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | A module to run static-sites that are fetched with Git and served with
-- nginx.
--
-- You should import this module qualified for better readability:
-- StaticSite.gitCloned reads weel.
module Devops.StaticSite (
    gitCloned
  , gitClonedInParasite
  ) where

import           Control.Distributed.Closure    (Closure, cap, cmap, cpure)
import           Control.Distributed.Closure.TH
import qualified Data.ByteString.Lazy           as Lazy
import qualified Data.Text                      as Text
import           System.FilePath.Posix          (takeFileName)

import           Devops.Debian.Commands
import           Devops.Debian.User
import           Devops.Git
import           Devops.Networking
import           Devops.Nginx
import           Devops.Parasite
import           Devops.Storage
import           Devops.Base

-- | Clone a static site from a git repository.
--
-- In current implementation, the .git directory from the clone will be served,
-- hence make sure the GitRepo you clone has no secret.
gitCloned :: DevOp GitRepo -> DevOp NginxServerConfig
gitCloned mkRepo = do
    let locs = [ NginxLocationConfig "/" staticSiteDirectives ]
    (GitRepo dir@(DirectoryPresent dirname) _ _) <- mkRepo
    return $ NginxServerConfig 80 (Text.pack $ takeFileName dirname) dir "index.html" locs

-- | Helper function.
--
-- Clones multiple static sites in a homedir for a user named 'staticsites' and
-- starts nginx to serve these sites. This function is defined at top level to
-- be useful as a static pointer.
_gitClonedStaticSites :: ConfigDir
                      -> [(HostString,GitUrl,GitBranch)]
                      -> DevOp (Listening WebService)
_gitClonedStaticSites rundir infos = do
    let dir hostdir = userDirectory (Text.unpack hostdir) (mereUser "staticsites")
    let repo (hostname,url,branch) = gitClone url branch git (dir hostname)
    let configs = map (gitCloned . repo) infos
    let srv = nginxMainServer 80 rundir configs
    (fmap . fmap) (const WebService) srv

-- | Serve Git-cloned static-sites in a parasite.
gitClonedInParasite :: ConfigDir
                   -> [(HostString,GitUrl,GitBranch)]
                   -> DevOp (ParasitedHost)
                   -> (Lazy.ByteString -> [String])
                   -> [DevOp NginxServerConfig]
gitClonedInParasite rundir gitInfo host fArgs =
    map remoteConfig gitInfo
  where
    staticSiteClosure :: Closure (DevOp (Listening WebService))
    staticSiteClosure = cap (cmap (static _gitClonedStaticSites)
                                  (cpure $cdict rundir))
                            (cpure $cdict gitInfo)

    remoteHttp :: DevOp (Remoted (Listening WebService))
    remoteHttp =
        (fmap . fmap . fmap) (const WebService) (remoted staticSiteClosure fArgs host)

    remoteConfig :: (HostString, GitUrl, GitBranch) -> DevOp NginxServerConfig
    remoteConfig (hostname,_,_) =
        proxyPass rundir hostname remoteHttp
