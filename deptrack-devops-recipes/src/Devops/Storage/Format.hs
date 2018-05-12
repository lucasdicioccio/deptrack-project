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

newtype Formatted a = Formatted { unFormat :: a }

type MB a = a
data PartitionType = LinuxSwap | Ext3

data Partition = Partition {
      _start :: MB Int
    , _stop  :: MB Int
    , _type  :: PartitionType
    }

newtype Schema = Schema { getPartitions :: [ Partition ] }

partition :: Typeable a
    => Schema -> DevOp env (BlockDevice a) -> DevOp env (Partitioned (BlockDevice a))
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
namedPartitionPath :: NamedPartition -> FilePath
namedPartitionPath = snd

formatDevice :: DevOp env (Partitioned (BlockDevice a))
             -> DevOp env (Formatted (Partitioned (BlockDevice a)))
formatDevice mkparted = do
    dev <- mkparted
    let partitions = fmap pure (namedPartitions dev)
    _ <- traverse formatPartition partitions `inject` mkparted
    return (Formatted dev)
  
namedPartitions :: Partitioned (BlockDevice a) -> [NamedPartition]
namedPartitions (Partitioned schema (BlockDevice _ path)) =
    zip (getPartitions schema) (fmap path [1..])

formatPartition :: DevOp env NamedPartition -> DevOp env (Formatted NamedPartition)
formatPartition mkPart = devop (Formatted) mkOp $ do
    np@(Partition _ _ pType, _) <- mkPart
    case pType of
        LinuxSwap -> formatSwapPartition (namedPartitionPath <$> mkPart)
        Ext3      -> formatExt3Partition (namedPartitionPath <$> mkPart)
    return np
  where
    mkOp (_,path) = runPreOp $
        noop ("format-partition: " <> Text.pack path)
             ("Formats " <> Text.pack path<> "using the right filesystem")

formatSwapPartition :: DevOp env FilePath -> DevOp env ()
formatSwapPartition mkpath = devop (const ()) mkOp $ do
    (,) <$> mkswap <*> mkpath
  where
    mkOp (swap, path) =
        buildOp ("mkswap:" <> Text.pack path)
                ("makes a fresh swap partition")
                (noCheck)
                (blindRun swap [path] "")
                (noAction)
                (noAction)

formatExt3Partition :: DevOp env FilePath -> DevOp env ()
formatExt3Partition mkpath = devop (const ()) mkOp $ do
    (,) <$> mkfsExt3 <*> mkpath
  where
    mkOp (ext3, path) =
        buildOp ("mkfs.ext3:" <> Text.pack path)
                ("makes a fresh ext3 partition")
                (noCheck)
                (blindRun ext3 [path] "")
                (noAction)
                (noAction)
