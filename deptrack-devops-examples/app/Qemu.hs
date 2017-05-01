{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (void)
import           Data.String.Conversions (convertString)
import           System.Environment (getArgs)

import           Devops.Base (DevOp)
import           Devops.Cli (defaultMain)
import           Devops.Optimize (optimizeDebianPackages)
import           Devops.Networking
import           Devops.Qemu
import           Devops.QemuBootstrap
import           Devops.QemuNbd
import           Devops.BaseImage
import           Devops.Callback
import           Devops.Service
import           Devops.Storage

main :: IO ()
main = do
    args <- getArgs
    if isMagicArgv args then chrootNestedSetup (convertString $ head args) else vmSetup args
  where
    vmSetup :: [String] -> IO ()
    vmSetup (pubkeypath:args) = do
        self <- readSelf
        pubkeys <- readPubkeyContent pubkeypath
        defaultMain (vm self pubkeys) [optimizeDebianPackages] args

    chrootNestedSetup :: PubkeyContent -> IO ()
    chrootNestedSetup pubkeys = do
        defaultMain (nbdBootstrapConfig nbdSlot (baseImageConfig pubkeys) imageContent)
                    [optimizeDebianPackages]
                    ["up"]

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

vm :: Self -> PubkeyContent -> DevOp (Daemon QemuVM)
vm self pubkeys = qemuVm (rundir, repo) addressPlan index ram cpus eth0 disk
  where
    disk :: DevOp QemuDisk
    disk = newDiskFromBaseImage rundir index baseImage

    baseImage :: DevOp (BaseImage QemuBase)
    baseImage = dirtyBootstrap "/opt/repo/debootstrap"
                               savedImagePath
                               nbdSlot
                               (baseImageConfig pubkeys)
                               imgSize
                               callback

    callback :: BinaryCall
    callback = BinaryCall self (const $ magicArgv pubkeys)

baseImageConfig :: PubkeyContent -> (BaseImageConfig QemuBase)
baseImageConfig pubkeys =
    BaseImageConfig bootstrapBin (xenial imgSuperUser pubkeys)

imageContent :: DevOp ()
imageContent = return ()

magicArgv pubkeys = (convertString pubkeys : magicArgvTail)
magicArgvTail = ["*", "*", "*"]
isMagicArgv args = drop 1 args == magicArgvTail

type Self = FilePath

readSelf :: IO Self
readSelf = takeWhile (/= '\NUL') <$> readFile "/proc/self/cmdline"

type PubkeyContent = FileContent

readPubkeyContent :: FilePath -> IO PubkeyContent
readPubkeyContent path = convertString <$> readFile path
