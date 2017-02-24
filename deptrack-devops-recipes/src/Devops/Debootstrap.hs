{-# LANGUAGE OverloadedStrings #-}

module Devops.Debootstrap (
    Debootstrapped (..)
  , debootstrapped
  , DebootstrapSuite (..)
  , trusty, xenial , jessie
  ) where

import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           System.FilePath         ((</>))

import           Devops.Base
import qualified Devops.Debian.Commands as Cmd
import           Devops.Storage
import           Devops.Utils

-- | The configuration for the suite to debootstrap.
data DebootstrapSuite a =
    DebootstrapSuite { suiteName                 :: !Name
                     , mirror                    :: !Mirror
                     , specificConfiguration     :: !a
                     }

-- | A Mirror to debootstrap from.
newtype Mirror = Mirror { mirrorURL :: Text }

-- | Some well-known mirrors.
ubuntuMirror, debianMirror :: Mirror
ubuntuMirror = Mirror "http://archive.ubuntu.com/ubuntu/"
debianMirror = Mirror "http://httpredir.debian.org/debian/"

-- | Some well-known debootstrap suites.
trusty, xenial, jessie :: a -> DebootstrapSuite a
trusty = DebootstrapSuite "trusty" ubuntuMirror
xenial = DebootstrapSuite "xenial" ubuntuMirror
jessie = DebootstrapSuite "jessie" debianMirror

-- | A directory holding a Debootstrapped environment.
data Debootstrapped = Debootstrapped !DirectoryPresent

-- | Debootstraps a distribution in the root partition of an NBD export.
-- Also mounts /dev, /sys, and /proc in the deboostrapped directory.
-- TODO: the extra mounts should be optional
debootstrapped :: DebootstrapSuite a
  -> DevOp DirectoryPresent
  -> DevOp Debootstrapped
debootstrapped suite mkTarget = devop fst mkOp $ do
    let args mntdir = [ "--variant", "buildd"
                      , "--arch", "amd64"
                      , "--include", "openssh-server,sudo,ntp,libgmp-dev,grub-pc"
                      , Text.unpack (suiteName suite)
                      , mntdir
                      , Text.unpack (mirrorURL $ mirror suite)
                      ]
    dir@(DirectoryPresent mntdir) <- mkTarget
    mnt <- Cmd.mount
    umnt <- Cmd.umount
    dstrap <- Cmd.debootstrap
    cr <- Cmd.chroot
    return (Debootstrapped dir, (dstrap, cr, mnt, umnt, mntdir, args))
  where
    mkOp (_, (dstrap, cr, mnt, umnt, mntdir, args)) =
        buildOp
            ("debootstrap:" <> suiteName suite)
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
