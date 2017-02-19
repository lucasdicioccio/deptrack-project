{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Devops.Storage.Format where

import           Control.Monad           (void)
import           Data.Monoid             ((<>))
import qualified Data.Text               as Text
import           Data.Typeable           (Typeable)

import           Devops.Base
import           Devops.Debian.Commands
import           Devops.Storage.BlockDevice
import           Devops.Utils

newtype Partitioned a = Partitioned a

type MB a = a
data PartitionType = LinuxSwap | Ext2

data Partition = Partition {
      _start :: MB Int
    , _stop  :: MB Int
    , _type  :: PartitionType
    }

newtype Schema = Schema { getPartitions :: [Partition] }

partition :: Typeable a
    => Schema -> DevOp (BlockDevice a) -> DevOp (Partitioned (BlockDevice a))
partition schema mkBlock = devop (Partitioned . fst) mkOp $ do
    blkdev <- mkBlock
    fdisk <- parted
    return (blkdev, fdisk)
  where
    mkOp (BlockDevice path, fdisk) =
        buildOp ("partition-block:" <> Text.pack path)
                ("re-partitions a disk using parted")
                (noCheck)
                (blindRun fdisk [ path , "mktable" , "msdos" ] "" >>
                 (void $ traverse (callParted fdisk path) (getPartitions schema)))
                (noAction)
                (noAction)
    callParted fdisk path = \case
        (Partition start end LinuxSwap) -> do
            blindRun fdisk [ "-s" , path
                           , "mkpart", "primary", "linux-swap"
                           , show start, show end
                           ]
                           ""
        (Partition start end Ext2) -> do
            blindRun fdisk [ "-s" , path
                           , "mkpart", "primary", "ext2"
                           , show start, show end
                           ]
                           ""
