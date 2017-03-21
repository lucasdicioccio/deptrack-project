{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}

module Main where

import           Control.Distributed.Closure
import           Control.Monad (void)
import           Data.String.Conversions (convertString)
import           System.Environment (getArgs)

import           Devops.Base (DevOp)
import           Devops.BaseImage
import           Devops.Callback
import           Devops.Cli (defaultMain, opFromClosureB64)
import           Devops.Debian (deb)
import qualified Devops.Debian.Commands as Cmd
import           Devops.Docker
import           Devops.DockerBootstrap
import           Devops.Git (GitUrl, gitClone)
import           Devops.Nginx
import           Devops.Optimize (optimizeDebianPackages)
import qualified Devops.StaticSite as StaticSite
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
        defaultMain (dock self) [optimizeDebianPackages] args

readSelf :: IO SelfPath
readSelf = takeWhile (/= '\NUL') <$> readFile "/proc/self/cmdline"

magicChrootArgv = ["*", "*", "*"]
isMagicChrootArgv args = args == magicChrootArgv

magicDockerArgv = "~~~"
isMagicDockerArgv args = take 1 args == [magicDockerArgv]

tempdir = "/opt/dockbootstrap-website"
bootstrapBin = "/sbin/bootstrap-deptrack-devops-website"

dock :: SelfPath -> DevOp ()
dock self = void $ do
    let image = dockerImage "deptrack-dockerized-website-example" (simpleBootstrap tempdir baseImageConfig chrootCallback)
    -- a nifty callback where we pull arbitrary stuff in
    dockerized "deptrack-devops-example-dockerized-website"
               (selfCallback self magicDockerArgv)
               image
               (closure $ static dockerDevOpContent)
  where
    chrootCallback :: CallBackMethod
    chrootCallback = BinaryCall self magicChrootArgv

    baseImageConfig :: BaseImageConfig DockerBase
    baseImageConfig =
        BaseImageConfig bootstrapBin xenial

chrootImageContent :: DevOp ()
chrootImageContent = void $ do
    deb "git-core"

dockerDevOpContent :: DevOp ()
dockerDevOpContent = void $ do
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
