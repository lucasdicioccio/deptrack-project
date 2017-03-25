{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           Control.Distributed.Closure
import           Control.Monad (void)
import           Data.String.Conversions (convertString)
import           System.Environment (getArgs)

import           Devops.Base
import           Devops.BaseImage
import           Devops.Callback
import           Devops.Cli (defaultMain, opFromClosureB64)
import           Devops.Debian (deb)
import qualified Devops.Debian.Commands as Cmd
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

main :: IO ()
main = do
    args <- getArgs
    go args
  where
    go xs | isMagicChrootArgv xs = chrootNestedSetup
          | isMagicDockerArgv xs = base64EncodedNestedSetup (drop 1 xs)
          | otherwise            = dockerSetup xs

    chrootNestedSetup :: IO ()
    chrootNestedSetup = void $ do
        defaultMain chrootImageContent [optimizeDebianPackages] ["up"]

    base64EncodedNestedSetup :: [String] -> IO ()
    base64EncodedNestedSetup (b64:[]) = void $ do
        let target = opFromClosureB64 (convertString b64)
        defaultMain target [optimizeDebianPackages] ["upkeep"]
    base64EncodedNestedSetup _ = error "invalid args for magic docker callback"

    dockerSetup :: [String] -> IO ()
    dockerSetup args = do
        self <- readSelf
        let f c ys = defaultMain c [optimizeDebianPackages] ys
        let eval c = OpFunctions noCheck (f c ["up"]) (f c ["down"]) noAction
        f (dock self eval) args

readSelf :: IO SelfPath
readSelf = takeWhile (/= '\NUL') <$> readFile "/proc/self/cmdline"

magicChrootArgv = ["*", "*", "*"]
isMagicChrootArgv args = args == magicChrootArgv

magicDockerArgv = "~~~"
isMagicDockerArgv args = take 1 args == [magicDockerArgv]

tempdir = "/opt/dockbootstrap-website"
bootstrapBin = "/sbin/bootstrap-deptrack-devops-website"

dock :: SelfPath -> Evaluator OpFunctions -> DevOp ()
dock self eval = void $ do
    let image = dockerImage "deptrack-dockerized-website-example" (simpleBootstrap tempdir baseImageConfig chrootCallback)
    -- a nifty callback where we pull arbitrary stuff in
    let d = dockerizedDaemon "deptrack-devops-example-dockerized-website"
                             (selfCallback self magicDockerArgv)
                             image
                             (closure $ static dockerDevOpContent)
    let d' = delay (resolveDockerRemote d) (mainNginxProxy . adapt)
    delayedEval d' eval
  where
    adapt = fmap exposed2listening . (fmap . fmap) nginxAsWebService
    chrootCallback :: CallBackMethod
    chrootCallback = BinaryCall self magicChrootArgv

    baseImageConfig :: BaseImageConfig DockerBase
    baseImageConfig =
        BaseImageConfig bootstrapBin xenial

mainNginxProxy :: Remoted (Listening WebService) -> DevOp (Exposed (Daemon Nginx))
mainNginxProxy upstream = do
    let cfgdir = "/opt/rundir" 
    let cfg = proxyPass cfgdir "dicioccio.fr" (return upstream)
    reverseProxy cfgdir [cfg]

chrootImageContent :: DevOp ()
chrootImageContent = void $ do
    deb "git-core"

dockerDevOpContent :: DevOp (Exposed (Daemon Nginx))
dockerDevOpContent =
    reverseProxy "/opt/rundir" nginxConfigs

nginxConfigs :: [DevOp NginxServerConfig]
nginxConfigs = [
    site "dicioccio.fr" "https://github.com/lucasdicioccio/site"
  , site "lubian.info" "https://github.com/lubian/site"
  ]

site :: HostName -> GitUrl -> DevOp NginxServerConfig
site host url = StaticSite.gitCloned repo
  where
    repo = gitClone url "master" Cmd.git (userDirectory (convertString host) user)
    user = mereUser "staticsites"
