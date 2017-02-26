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
import           Devops.Docker
import           Devops.DockerBootstrap
import           Devops.Optimize (optimizeDebianPackages)
import           Devops.Storage

import           Devops.Haskell
import           Devops.Debian.User

main :: IO ()
main = do
    args <- getArgs
    go args
    if isMagicChrootArgv args then chrootNestedSetup else dockerSetup args
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
        defaultMain target [optimizeDebianPackages] ["up"]
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

tempdir = "/opt/dockbootstrap"
bootstrapBin = "/sbin/bootstrap-deptrack-devops"

dock :: SelfPath -> DevOp ()
dock self = void $ do
    let image = dockerImage "deptrack-example" (simpleBootstrap tempdir baseImageConfig chrootCallback)

    -- a typical container where we copy the file before starting
    let mkCmd = return $ (ImportedContainerCommand (FilePresent "/usr/bin/touch") ["hello-world"])
    container "deptrack-devops-example-container"
              image
              mkCmd

    -- a nifty callback where we pull arbitrary stuff in
    dockerized "deptrack-devops-example-docker-callback"
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
    deb "python3"
    stackPackage "attoparsec" (mereUser "root")
