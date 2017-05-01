{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers    #-}

module Main where

import           Control.Distributed.Closure
import           Control.Monad (void)
import           DepTrack (inject)
import           Data.String.Conversions (convertString)
import           System.Environment (getArgs)

import           Devops.Base (DevOp)
import           Devops.BaseImage
import           Devops.Binary (Binary, HasBinary)
import           Devops.Callback
import           Devops.Cli (defaultMain, opClosureFromB64, opClosureToB64, SelfPath)
import           Devops.Debian (deb)
import           Devops.Docker
import           Devops.DockerBootstrap
import           Devops.Optimize (optimizeDebianPackages)
import           Devops.Postgre (libpqDev)
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
        let target = unclosure $ opClosureFromB64 (convertString b64) :: DevOp ()
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

tempdir = "/opt/dockbootstrap-docker-example"
bootstrapBin = "/sbin/bootstrap-deptrack-devops"

dock :: SelfPath -> DevOp ()
dock self = void $ do
    let image = dockerImage "deptrack-example-image" (simpleBootstrap tempdir baseImageConfig chrootCallback)

    -- a typical container where we copy the file before starting
    let mkCmd = return $ (ImportedContainerCommand (FilePresent "/usr/bin/touch") ["hello-world"])
    container "deptrack-devops-example-container-touch"
              Wait
              image
              mkCmd

    let dockerCallback clo =
            BinaryCall self (const $ magicDockerArgv:[convertString $ opClosureToB64 clo])

    -- a nifty callback where we pull arbitrary stuff in
    let artifact = dockerized "deptrack-devops-example-docker-callback-build"
                              image
                              (continue (closure $ static dockerDevOpContent)
                                        unclosure
                                        dockerCallback)

    -- committedImage artifact
    fetchFile "/opt/postgrest-bin" artifact
  where
    chrootCallback :: BinaryCall
    chrootCallback = BinaryCall self (const magicChrootArgv)

    baseImageConfig :: BaseImageConfig DockerBase
    baseImageConfig =
        BaseImageConfig bootstrapBin xenial

chrootImageContent :: DevOp ()
chrootImageContent = void $ do
    deb "git-core"

dockerDevOpContent :: DevOp FilePresent
dockerDevOpContent = do
    deb "python3"
    binaryPresent postgrest

postgrestProject :: DevOp (StackProject "postgrest")
postgrestProject = fmap fst $ do
    let url = "https://github.com/begriffs/postgrest.git"
    let branch = "master"
    let systemDependencies = libpqDev
    stackProject url branch "git-postgrest" (mereUser "user") `inject` systemDependencies

instance HasBinary (StackProject "postgrest") "postgrest" where

postgrest :: DevOp (Binary "postgrest")
postgrest = stackInstall "/home/user" postgrestProject
