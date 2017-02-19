{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Devops.Storage.Format where

import           Control.Monad           (void)
import           Data.Monoid             ((<>))
import qualified Data.Text               as Text

import           Devops.Base
import           Devops.Debian.Commands
import           Devops.QemuNbd
import           Devops.Utils

newtype Partitioned a = Partitioned a
newtype Formatted a = Formatted a
-- | Partitions an NBD-exported image.
-- TODO: pass partition plan as argument
-- TODO: relax type on NBDExport and work on any block device.
partitionWithFDisk :: DevOp NBDExport -> DevOp (Partitioned NBDExport)
partitionWithFDisk mkBlock = devop (Partitioned . fst) mkOp $ do
    let schema = unlines [ ",512,82"    -- linux swap, 512
                         , ",,,*"       -- bootable rest
                         , ""           -- pray
                         ]
    blkdev <- mkBlock
    fdisk <- sfdisk
    return (blkdev, (schema, fdisk))
  where
    mkOp (NBDExport slot _, (schema, fdisk)) =
        let path = nbdDevicePath slot in
        buildOp ("partition-ndb:" <> Text.pack path)
                ("re-partitions a disk")
                (noCheck)
                (blindRun fdisk [path, "-D", "-uM"] schema)
                (noAction)
                (noAction)

type MB a = a
data PartitionType = LinuxSwap | Ext2

partition :: DevOp NBDExport -> DevOp (Partitioned NBDExport)
partition mkBlock = devop (Partitioned . fst) mkOp $ do
    let schema = [(0, 512, LinuxSwap), (512, 20000, Ext2)] :: [(Int,Int,PartitionType)]
    blkdev <- mkBlock
    fdisk <- parted
    return (blkdev, (schema, fdisk))
  where
    mkOp (NBDExport slot _, (schema, fdisk)) =
        let path = nbdDevicePath slot in
        buildOp ("partition-ndb:" <> Text.pack path)
                ("re-partitions a disk using parted")
                (noCheck)
                (blindRun fdisk [ path , "mktable" , "msdos" ] "" >>
                 (void $ traverse (callParted fdisk path) schema))
                (noAction)
                (noAction)
    callParted fdisk path = \case
        (start, end, LinuxSwap) -> do
            blindRun fdisk [ "-s" , path , "mkpart", "primary", "linux-swap", show start, show end ] ""
        (start, end, Ext2) -> do
            blindRun fdisk [ "-s" , path , "mkpart", "primary", "ext2", show start, show end ] ""

