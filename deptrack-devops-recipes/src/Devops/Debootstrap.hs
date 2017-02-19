{-# LANGUAGE OverloadedStrings #-}

module Devops.Debootstrap where

import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           System.FilePath         ((</>))

import           Devops.Base
import qualified Devops.Debian.Commands as Cmd
import           Devops.Storage
import           Devops.Storage.Mount
import           Devops.Utils

data Debootstrapped = Debootstrapped !DirectoryPresent

-- | The configuration for the suite to debootstrap.
-- TODO: push more of the configuration here.
data DebootstrapSuite =
    DebootstrapSuite { suiteName                 :: Name
                     , kernelPackageName         :: Name
                     , dhcpClientPackageName     :: Name
                     , ethernetAdapterDchpConfig :: DevOp FilePresent
                     , mirror                    :: !Mirror
                     }

-- | A Mirror to debootstrap from.
newtype Mirror = Mirror { mirrorURL :: Text }

ubuntuMirror :: Mirror
ubuntuMirror = Mirror "http://archive.ubuntu.com/ubuntu/"

debianMirror :: Mirror
debianMirror = Mirror "http://httpredir.debian.org/debian/"

type DebootstrapTarget a = MountedPartition

-- | Debootstraps a distribution in the root partition of an NBD export.
-- Also mounts /dev, /sys, and /proc in the deboostrapped directory.
debootstrapped :: DebootstrapSuite
  -> FilePath
  -> DevOp (DebootstrapTarget a)
  -> DevOp Debootstrapped
debootstrapped suite dirname mkTarget = devop fst mkOp $ do
    let args mntdir = [ "--variant", "buildd"
                      , "--arch", "amd64"
                      , "--include", "openssh-server,sudo,ntp,libgmp-dev,grub-pc"
                      , Text.unpack (suiteName suite)
                      , mntdir
                      , Text.unpack (mirrorURL $ mirror suite)
                      ]
    dir@(DirectoryPresent mntdir) <- directory dirname
    _ <- mkTarget
    mnt <- Cmd.mount
    umnt <- Cmd.umount
    dstrap <- Cmd.debootstrap
    cr <- Cmd.chroot
    return (Debootstrapped dir, (dstrap, cr, mnt, umnt, mntdir, args))
  where
    mkOp (_, (dstrap, cr, mnt, umnt, mntdir, args)) =
        buildOp
            ("debootstrap")
            ("unpacks a clean intallation in" <> Text.pack mntdir)
            (noCheck)
            (blindRun dstrap (args mntdir) ""
             >> blindRun mnt ["-o", "bind", "/dev", mntdir </> "dev"] ""
             >> blindRun cr [mntdir, "mount", "-t", "proc", "none", "/proc"] ""
             >> blindRun cr [mntdir, "mount", "-t", "sysfs", "none", "/sys"] "")
            (blindRun cr [mntdir, "umount", "/proc"] ""
             >> blindRun cr [mntdir, "umount", "/sys"] ""
             >> blindRun umnt [mntdir </> "dev"] "")
            (noAction)
