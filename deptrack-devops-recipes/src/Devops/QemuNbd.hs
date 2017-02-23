{-# LANGUAGE OverloadedStrings #-}

module Devops.QemuNbd where

import           Data.Monoid            ((<>))
import qualified Data.Text              as Text

import           Devops.Debian.Commands
import           Devops.Linux           (kernelmodule)
import           Devops.Storage
import           Devops.Storage.BlockDevice
import           Devops.Base
import           Devops.Utils

type GB a = a
type Size = Int
type NBDSlot = Int -- TODO: constraint to, say, 16
type NBDPartitionSlot = Int
data NBDExport = NBDExport NBDSlot QemuImage

newtype QemuImage = QemuImage { getImage :: FilePresent }

-- | Creates a new QCOW2 image of a given size.
qcow2Image :: FilePath -> GB Size -> DevOp QemuImage
qcow2Image path gb = do
    let args = pure ["create", "-f", "qcow2", path, show gb <> "G"]
    (_,_,x) <- generatedFile path qemuImg args
    return (QemuImage x)

-- | Uses qemu-nbd to export and image on a given slot.
nbdMount :: NBDSlot -> DevOp QemuImage -> DevOp NBDExport
nbdMount slot mkImg = devop (NBDExport slot . fst) mkOp $ do
    _ <- kernelmodule "nbd" [("max_part","8")]
    img <- mkImg
    q <- qemuNbd
    return (img, q)
  where
    mkOp (QemuImage (FilePresent imagePath), q) =
        buildOp ("qemu-nbd-mount: " <> Text.pack imagePath)
                ("mounts a qemu image with nbd at " <> Text.pack (nbdDevicePath slot))
                (noCheck)
                (blindRun q ["-c", nbdDevicePath slot, imagePath] "")
                (blindRun q ["-d", nbdDevicePath slot] "")
                (noAction)

-- | Computes the block-device path for a given NBD device slot.
nbdDevicePath :: NBDSlot -> FilePath
nbdDevicePath n = "/dev/nbd" <> show n -- decimal no trailing zero

-- | Computes the block-device path for a given NBD device partition.
nbdPartitionPath :: NBDSlot -> NBDPartitionSlot -> FilePath
nbdPartitionPath n p = nbdDevicePath n <> "p" <> show p

-- | Computes the block-device object from a given NBD Export.
nbdDevice :: NBDExport -> BlockDevice NBDExport
nbdDevice (NBDExport slot _) = BlockDevice (nbdDevicePath slot) f
  where
    f :: NBDPartitionSlot -> FilePath
    f = nbdPartitionPath slot
