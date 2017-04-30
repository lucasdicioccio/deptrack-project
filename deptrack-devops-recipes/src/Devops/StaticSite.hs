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
  ) where

import qualified Data.Text                      as Text
import           System.FilePath.Posix          (takeFileName)

import           Devops.Debian.Commands
import           Devops.Debian.User
import           Devops.Git
import           Devops.Networking
import           Devops.Nginx
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
