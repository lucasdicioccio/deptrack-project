{-# LANGUAGE OverloadedStrings #-}

module Devops.Debootstrap where

import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           System.FilePath         ((</>))

import           Devops.Base
import           Devops.Debian.Commands
import           Devops.QemuNbd
import           Devops.Storage
import           Devops.Storage.Format
import           Devops.Utils

data Debootstrapped = Debootstrapped !DirectoryPresent

-- | The configuration for the suite to debootstrap.
-- TODO: push more of the configuration here.
data DebootstrapSuite =
    DebootstrapSuite { suiteName :: Name
                     , kernelPackageName :: Name
                     , dhcpClientPackageName :: Name
                     , ethernetAdapterDchpConfig :: DevOp FilePresent
                     , mirror    :: !Mirror
                     }

-- | A Mirror to debootstrap from.
newtype Mirror = Mirror { mirrorURL :: Text }

ubuntuMirror :: Mirror
ubuntuMirror = Mirror "http://archive.ubuntu.com/ubuntu/"

debianMirror :: Mirror
debianMirror = Mirror "http://httpredir.debian.org/debian/"


type DebootstrapDir = Formatted (Partitioned NBDExport)

-- | Debootstraps a distribution in the root partition of an NBD export.
-- Also mounts /dev, /sys, and /proc in the deboostrapped directory.
--
-- TODO: add a "Mounted" type to represent dev/sysfs/tmpfs/etc. mounts
debootstrapped :: DebootstrapSuite
  -> FilePath
  -> DevOp DebootstrapDir
  -> DevOp Debootstrapped
debootstrapped suite dirname mkPartitions = devop fst mkOp $ do
    let args mntdir = [ "--variant", "buildd"
                      , "--arch", "amd64"
                      , "--include", "openssh-server,sudo,ntp,libgmp-dev,grub-pc"
                      , Text.unpack (suiteName suite)
                      , mntdir
                      , Text.unpack (mirrorURL $ mirror suite)
                      ]
    dir@(DirectoryPresent mntdir) <- directory dirname
    (Formatted partitions) <- mkPartitions
    mnt <- mount
    umnt <- umount
    dstrap <- debootstrap
    cr <- chroot
    return (Debootstrapped dir, (partitions, dstrap, cr, mnt, umnt, mntdir, args))
  where
    mkOp (_, (blockdev, dstrap, cr, mnt, umnt, mntdir, args)) =
        buildOp
            ("debootstrap")
            ("unpacks a clean intallation in" <> Text.pack mntdir)
            (noCheck)
            (blindRun mnt [rootPartitionPath blockdev, mntdir] ""
             >> blindRun dstrap (args mntdir) ""
             >> blindRun mnt ["-o", "bind", "/dev", mntdir </> "dev"] ""
             >> blindRun cr [mntdir, "mount", "-t", "proc", "none", "/proc"] ""
             >> blindRun cr [mntdir, "mount", "-t", "sysfs", "none", "/sys"] "")
            (blindRun cr [mntdir, "umount", "/proc"] ""
             >> blindRun cr [mntdir, "umount", "/sys"] ""
             >> blindRun umnt [mntdir </> "dev"] ""
             >> blindRun umnt [rootPartitionPath blockdev] "")
            (noAction)

-- | Path to the swap partition of the NBD-exported image.
swapPartitionPath :: Partitioned NBDExport -> FilePath
swapPartitionPath (Partitioned (NBDExport n _)) = nbdPartitionPath n 1

-- | Path to the root partition of the NBD-exported image.
rootPartitionPath :: Partitioned NBDExport -> FilePath
rootPartitionPath (Partitioned (NBDExport n _)) = nbdPartitionPath n 2

