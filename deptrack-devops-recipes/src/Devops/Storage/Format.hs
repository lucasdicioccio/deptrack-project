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

data Partitioned a = Partitioned {
      partitionSchema :: Schema
    , unPartition :: a
    }

newtype Formatted a = Formatted a

type MB a = a
data PartitionType = LinuxSwap | Ext3

data Partition = Partition {
      _start :: MB Int
    , _stop  :: MB Int
    , _type  :: PartitionType
    }

newtype Schema = Schema { getPartitions :: [ Partition ] }

partition :: Typeable a
    => Schema -> DevOp (BlockDevice a) -> DevOp (Partitioned (BlockDevice a))
partition schema mkBlock = devop (Partitioned schema . fst) mkOp $ do
    blkdev <- mkBlock
    fdisk <- parted
    return (blkdev, fdisk)
  where
    mkOp (BlockDevice path _, fdisk) =
        buildOp ("partition-block:" <> Text.pack path)
                ("re-partitions a disk using parted")
                (noCheck)
                (blindRun fdisk [ path , "mktable" , "msdos" ] "" >>
                 (void $ traverse (callParted fdisk path) (getPartitions schema)))
                (noAction)
                (noAction)
    callParted fdisk path (Partition start end ptype) = do
            blindRun fdisk [ "-s" , path
                           , "mkpart", "primary", partedFormat ptype
                           , show start, show end
                           ]
                           ""

partedFormat :: PartitionType -> String
partedFormat = \case
    LinuxSwap -> "linux-swap"
    Ext3      -> "ext2"

type NamedPartition = (Partition, FilePath)

formatDevice :: DevOp (Partitioned (BlockDevice a))
             -> DevOp (Formatted (Partitioned (BlockDevice a)))
formatDevice mkdevice = do
    dev <- mkdevice
    let pairs = f dev
    _ <- traverse formatPartition pairs
    return (Formatted dev)
  where
    f :: Partitioned (BlockDevice a) -> [NamedPartition]
    f (Partitioned schema (BlockDevice _ path)) = zip (getPartitions schema) (fmap path [1..])

formatPartition :: NamedPartition -> DevOp (Formatted NamedPartition)
formatPartition np@(Partition _ _ pType, path) = devop (Formatted) mkOp $ do
    case pType of
        LinuxSwap -> formatSwapPartition path >> return np
        Ext3      -> formatExt3Partition path >> return np
  where
    mkOp _ = runPreOp $
        noop ("format-partition: " <> Text.pack path)
             ("Formats " <> Text.pack path<> "using the right filesystem")

formatSwapPartition :: FilePath -> DevOp ()
formatSwapPartition path = devop (const ()) mkOp $ do
    mkswap
  where
    mkOp swap =
        buildOp ("mkswap:" <> Text.pack path)
                ("makes a fresh swap partition")
                (noCheck)
                (blindRun swap [path] "")
                (noAction)
                (noAction)

formatExt3Partition :: FilePath -> DevOp ()
formatExt3Partition path = devop (const ()) mkOp $ do
    mkfsExt3
  where
    mkOp ext3 =
        buildOp ("mkfs.ext3:" <> Text.pack path)
                ("makes a fresh ext3 partition")
                (noCheck)
                (blindRun ext3 [path] "")
                (noAction)
                (noAction)
