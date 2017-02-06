{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (void)
import           System.Environment (getArgs)

import           Devops.Base (DevOp)
import           Devops.Cli (defaultMain)
import           Devops.Optimize (optimizeDebianPackages)
import           Devops.Networking
import           Devops.Qemu
import           Devops.QemuNbd
import           Devops.BaseImage
import           Devops.Callback
import           Devops.Service
import           Devops.Storage

main :: IO ()
main = do
    args <- getArgs
    if args == magicArgv then chrootNestedSetup else vmSetup args
  where
    vmSetup :: [String] -> IO ()
    vmSetup args = do
        self <- readSelf
        defaultMain (vm self) [optimizeDebianPackages] args

    chrootNestedSetup :: IO ()
    chrootNestedSetup = do
        defaultMain (bootstrapConfig nbdSlot baseImageConfig imageContent) [optimizeDebianPackages] ["up"]

-- running-vm-related
repo = "/opt/repo"
rundir = "/opt/rundir"
addressPlan = tenspaceAddressPlan 42
index = 7
ram = 1024
cpus = 2

-- base-image-related
nbdSlot = 3
savedImagePath = "/opt/repo/deptrack-example.qemu"
bootstrapBin = "/sbin/bootstrap-deptrack-devops" 
imgSize = 20
imgSuperUser = "superuser"
pubkeys = "# PASTE HERE YOUR SUPERUSER PUBLIC KEY #"

vm :: Self -> DevOp (Daemon QemuVM)
vm self = qemuVm (rundir, repo) addressPlan index ram cpus disk
  where
    disk :: DevOp QemuDisk
    disk = newDiskFromBaseImage rundir index baseImage

    baseImage :: DevOp BaseImage
    baseImage = dirtyBootstrap "/opt/repo/debootstrap" savedImagePath nbdSlot baseImageConfig callback

    callback :: CallBackMethod
    callback = BinaryCall self magicArgv

baseImageConfig :: BaseImageConfig
baseImageConfig = BaseImageConfig imgSuperUser imgSize pubkeys bootstrapBin xenial

imageContent :: DevOp ()
imageContent = return ()

magicArgv = ["*", "*", "*"]

type Self = FilePath

readSelf :: IO Self
readSelf = takeWhile (/= '\NUL') <$> readFile "/proc/self/cmdline"
