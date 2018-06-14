{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           Control.Distributed.Closure
import           Control.Monad (guard, void)
import           Data.Traversable (sequence)
import           Data.Maybe (isJust)
import           Data.String.Conversions (convertString)
import           System.Environment (getArgs)

import           Devops.Base
import           Devops.BaseImage
import           Devops.Callback
import           Devops.Cli
import           Devops.Constraints (HasOS(..))
import           Devops.Debian (deb)
import qualified Devops.Debian.Commands as Debian
import           Devops.Docker
import           Devops.DockerBootstrap
import           Devops.Git (GitUrl, gitClone)
import           Devops.Networking
import           Devops.Nginx
import           Devops.Optimize (optimizeDebianPackages)
import           Devops.Ref
import qualified Devops.StaticSite as StaticSite
import           Devops.Service
import           Devops.Storage

import           Devops.Haskell
import           Devops.Debian.User

--------------------------------------------------------------------

data NoEnv = NoEnv
instance HasOS NoEnv where
  os _ = "debian"

-- | Represents the main entry point in our function.
data Stage =
    LocalHost
  -- ^ The binary was called (e.g., by a human) on the local-host.
  | InChroot
  -- ^ The binary was called in the chroot for configuring the base image.
  | InDocker
  -- ^ The binary was called in docker as a main program.

parseStage :: [String] -> (Stage, Method)
parseStage = \case
    ("_chroot_":arg:[]) -> (InChroot, appMethod arg)
    ("_docker_":arg:[]) -> (InDocker, appMethod arg)
    (arg:[])            -> (LocalHost, appMethod arg)
    args                -> error $ "unparsed args: " ++ show args

unparseStage :: Stage -> Method -> [String]
unparseStage stage m = case stage of
    LocalHost -> [ methodArg m ]
    InChroot  -> [ "_chroot_", methodArg m ]
    InDocker  -> [ "_docker_", methodArg m ]

--------------------------------------------------------------------

main :: IO ()
main = do
    let app = App parseStage unparseStage stages [optimizeDebianPackages] (\_ -> return NoEnv) :: App NoEnv Stage
    appMain app


--------------------------------------------------------------------
tempdir, bootstrapBin :: FilePath
tempdir = "/opt/dockbootstrap-website"
bootstrapBin = "/sbin/bootstrap-deptrack-devops-website"

stages :: Stage -> SelfPath -> (Stage -> Method -> [String]) -> DevOp NoEnv ()
stages InChroot _ _           = chrootImageContent
stages InDocker _ _           = void $ dockerDevOpContent
stages LocalHost self fixCall = void $ do
    -- prepare callbacks for binary calls
    let chrootCallback = binaryCall self (fixCall InChroot)
    let dockerCallback = binaryCall self (fixCall InDocker)

    -- prepare callback for delayed call
    let evalDelay c = OpFunctions noCheck (miniMain c ["up"]) (miniMain c ["down"]) noAction

    -- the base image
    let image = dockerImage "deptrack-dockerized-website-example"
                            (simpleBootstrap tempdir baseImageConfig chrootCallback)

    -- the website running in docker
    let website = dockerizedDaemon "deptrack-devops-example-dockerized-website"
                                   image
                                   (continueConst dockerDevOpContent dockerCallback)
                                   NoEnv
                                   id -- no modifications before start

    -- the locally-running reverse proxy
    let localReverseProxy innerProxy = do
            -- we traverse the (Remote Maybe _) into a (Maybe Remote _)
            let maybeInner = sequence innerProxy
            guard (isJust maybeInner)
            let (Just nginx) = maybeInner
            let ws = (fmap . fmap) nginxAsWebService nginx
            mainNginxProxy $ fmap exposed2listening ws

    delayedEval
        (delay (resolveDockerRemote website) localReverseProxy)
        evalDelay
        NoEnv
  where
    miniMain c ys = simpleMain c [optimizeDebianPackages] ys NoEnv
    baseImageConfig = BaseImageConfig bootstrapBin xenial

mainNginxProxy :: Remoted (Listening WebService) -> DevOp env (Exposed (Daemon Nginx))
mainNginxProxy upstream = do
    let cfgdir = "/opt/rundir"
    let cfg = proxyPass cfgdir "dicioccio.fr" (return upstream)
    reverseProxy cfgdir [cfg]

chrootImageContent :: DevOp env ()
chrootImageContent = void $ do
    deb "git-core"

dockerDevOpContent :: DevOp env (Exposed (Daemon Nginx))
dockerDevOpContent =
    reverseProxy "/opt/rundir" nginxConfigs

nginxConfigs :: [DevOp env NginxServerConfig]
nginxConfigs = [
    site "dicioccio.fr" "https://github.com/lucasdicioccio/site"
  , site "lubian.info" "https://github.com/lubian/site"
  ]

site :: HostName -> GitUrl -> DevOp env NginxServerConfig
site host url = StaticSite.gitCloned repo
  where
    repo = gitClone url "master" Debian.git (userDirectory (convertString host) user)
    user = mereUser "staticsites"
