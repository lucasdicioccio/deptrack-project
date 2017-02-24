{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (void)
import           System.Environment (getArgs)

import           Devops.Base (DevOp)
import           Devops.BaseImage
import           Devops.Callback
import           Devops.Cli (defaultMain)
import           Devops.DockerBootstrap
import           Devops.Optimize (optimizeDebianPackages)

main :: IO ()
main = do
    args <- getArgs
    if isMagicArgv args then chrootNestedSetup else dockerSetup args
  where
    chrootNestedSetup :: IO ()
    chrootNestedSetup = return ()

    dockerSetup :: [String] -> IO ()
    dockerSetup args = do
        self <- readSelf
        defaultMain (dock self) [optimizeDebianPackages] args

type Self = FilePath

readSelf :: IO Self
readSelf = takeWhile (/= '\NUL') <$> readFile "/proc/self/cmdline"

magicArgvTail = ["*", "*", "*"]
magicArgv = magicArgvTail
isMagicArgv args = args == magicArgvTail

tempdir = "/opt/dockbootstrap"
bootstrapBin = "/sbin/bootstrap-deptrack-devops"

dock :: Self -> DevOp ()
dock self = void $ simpleBootstrap tempdir baseImageConfig callback
  where
    callback :: CallBackMethod
    callback = BinaryCall self magicArgv

    baseImageConfig :: BaseImageConfig DockerBase
    baseImageConfig =
        BaseImageConfig (error "imgSuperUser") (error "pubkeys") bootstrapBin xenial
