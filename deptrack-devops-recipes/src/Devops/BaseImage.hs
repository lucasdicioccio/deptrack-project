{-# LANGUAGE OverloadedStrings #-}

module Devops.BaseImage where

import           Control.Monad           (void)
import           Data.Monoid             ((<>))
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           DepTrack                (declare, inject)
import           System.FilePath         (makeRelative, (</>))

import           Devops.Callback
import           Devops.Debian
import           Devops.Debian.Commands
import           Devops.Debian.User
import           Devops.QemuNbd
import           Devops.Storage
import           Devops.Base
import           Devops.Utils

data BaseImage = BaseImage {
    imagePath :: !FilePresent
  , config    :: !BaseImageConfig
  }
data BaseImageConfig = BaseImageConfig {
    superUser :: !Name
  , imageSize :: !(GB Size)
  , pubKeys   :: !FileContent
  , binPath   :: !FilePath -- Path to binary to turnup a new base image.
  , suite     :: !DebootstrapSuite
  }

newtype Partitioned a = Partitioned a
newtype Formatted a = Formatted a
data Debootstrapped = Debootstrapped !DirectoryPresent

-- | Partitions an NBD-exported image.
-- TODO: pass partition plan as argument
-- TODO: relax type on NBDExport and work on any block device.
partition :: DevOp NBDExport -> DevOp (Partitioned NBDExport)
partition mkBlock = devop (Partitioned . fst) mkOp $ do
  let schema = unlines [ ",512,82"    -- linux swap, 512
                       , ",,,*"       -- bootable rest
                       , ""           -- pray
                       ]
  blkdev <- mkBlock
  fdisk <- sfdisk
  return (blkdev, (schema, fdisk))
  where mkOp (NBDExport slot _, (schema, fdisk)) =
                 let path = nbdDevicePath slot in
                 buildOp ("partition-ndb:" <> Text.pack path)
                         ("re-partitions a disk")
                         (noCheck)
                         (blindRun fdisk [path, "-D", "-uM"] schema)
                         (noAction)
                         (noAction)

-- | Path to the swap partition of the NBD-exported image.
swapPartitionPath :: Partitioned NBDExport -> FilePath
swapPartitionPath (Partitioned (NBDExport n _)) = nbdPartitionPath n 1

-- | Path to the root partition of the NBD-exported image.
rootPartitionPath :: Partitioned NBDExport -> FilePath
rootPartitionPath (Partitioned (NBDExport n _)) = nbdPartitionPath n 2

-- | Formats the NBD-exported image partitions.
-- TODO: use preferences for the filesystem type.
formatted :: DevOp (Partitioned NBDExport) -> DevOp (Formatted (Partitioned NBDExport))
formatted mkNbdmount = devop (Formatted . fst) mkOp $ do
  swap <- mkswap
  mkfs <- mkfsExt3
  blockdev@(Partitioned (NBDExport slot _)) <- mkNbdmount
  return (blockdev, (slot,swap,mkfs))
  where mkOp (blockdev, (slot,swap,mkfs)) =
                let path = nbdDevicePath slot in
                buildOp ("format-nbd:" <> Text.pack path)
                        ("re-formats a disk")
                        (noCheck)
                        (blindRun swap [swapPartitionPath blockdev] ""
                         >> blindRun mkfs [rootPartitionPath blockdev] "")
                        (noAction)
                        (noAction)

-- | The configuration for the suite to debootstrap.
-- TODO: push more of the configuration here.
data DebootstrapSuite =
    DebootstrapSuite { suiteName :: Name
                     , kernelPackageName :: Name
                     , dhcpClientPackageName :: Name
                     , ethernetAdapterDchpConfig :: DevOp FilePresent
                     , mirror    :: !Mirror
                     }

trusty :: DebootstrapSuite
trusty = DebootstrapSuite "trusty" "linux-signed-image-generic-lts-trusty" "dhcp-client" eth0 ubuntuMirror
  where
    eth0 = fmap snd $ fileContent "/etc/network/interfaces.d/eth0" (pure eth0IfaceConfig)

xenial :: DebootstrapSuite
xenial = DebootstrapSuite "xenial" "linux-signed-image-generic-lts-xenial" "isc-dhcp-client" ens3 ubuntuMirror
  where
    ens3 = fmap snd $ fileContent "/etc/network/interfaces.d/ens3" (pure ens3IfaceConfig)
  
jessie :: DebootstrapSuite
jessie = DebootstrapSuite "jessie" "linux-image-amd64" "isc-dhcp-client" eth0 debianMirror
  where
    eth0 = fmap snd $ fileContent "/etc/network/interfaces.d/eth0" (pure eth0IfaceConfig)
  
-- | A Mirror to debootstrap from.
newtype Mirror = Mirror { mirrorURL :: Text }

ubuntuMirror :: Mirror
ubuntuMirror = Mirror "http://archive.ubuntu.com/ubuntu/"

debianMirror :: Mirror
debianMirror = Mirror "http://httpredir.debian.org/debian/"

-- | Debootstraps a distribution in the root partition of an NBD export.
-- Also mounts /dev, /sys, and /proc in the deboostrapped directory.
--
-- TODO: add a "Mounted" type to represent dev/sysfs/tmpfs/etc. mounts
debootstrapped :: DebootstrapSuite
  -> FilePath
  -> DevOp (Formatted (Partitioned NBDExport))
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
  where mkOp (_, (blockdev, dstrap, cr, mnt, umnt, mntdir, args)) = buildOp
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

-- | Bootstraps a base image, copying the image after turndown.
bootstrap :: FilePath  -- Path to a temporary dir receiving the debootstrap environment.
          -> FilePath  -- Path receiving the qemu image.
          -> NBDSlot   -- NBD device number to use during the bootstrap.
          -> BaseImageConfig  -- Configuration of the base image.
          -> CallBackMethod
          -> DevOp BaseImage
bootstrap = bootstrapWithStore (\x y -> fmap snd (turndownfileBackup x y))

-- | Bootstraps a base image, copying the image after turnup.
dirtyBootstrap :: FilePath  -- Path to a temporary dir receiving the debootstrap environment.
          -> FilePath  -- Path receiving the qemu image.
          -> NBDSlot   -- NBD device number to use during the bootstrap.
          -> BaseImageConfig  -- Configuration of the base image.
          -> CallBackMethod
          -> DevOp BaseImage
dirtyBootstrap = bootstrapWithStore (\x y -> fmap snd (turnupfileBackup x y))

bootstrapWithStore :: (FilePath -> DevOp FilePresent -> DevOp FilePresent)
          -> FilePath  -- Path to a temporary dir receiving the debootstrap environment.
          -> FilePath  -- Path receiving the qemu image.
          -> NBDSlot   -- NBD device number to use during the bootstrap.
          -> BaseImageConfig  -- Configuration of the base image.
          -> CallBackMethod
          -> DevOp BaseImage
bootstrapWithStore store dirname imgpath slot cfg (BinaryCall selfPath selfBootstrapArgs) = do
  let qcow = qcow2Image (imgpath <> ".tmp") (imageSize cfg)
  let backedupQcow = QemuImage <$> store imgpath (fmap getImage qcow)
  let src = localRepositoryFile selfPath
  let base = debootstrapped (suite cfg) dirname (formatted (partition (nbdMount slot qcow)))
  let makeDestPath (Debootstrapped (DirectoryPresent x)) = x </> (makeRelative "/" (binPath cfg))
  let desc1 = "copies " <> Text.pack selfPath <> " in config chroot for " <> Text.pack imgpath
  let copy = declare (noop "ready-to-configure" desc1) $ do
               fileCopy (makeDestPath <$> base) src
  let main = devop fst mkOp $ do
        (Debootstrapped (DirectoryPresent mntPath)) <- base
        _ <- copy
        cr <- chroot
        return (BaseImage (FilePresent imgpath) cfg, (cr,mntPath))
  fmap snd (backedupQcow `inject` main)
  where mkOp (_,(cr,mntPath)) = buildOp
                       ("bootstrap-configured") ("finalizes configuration")
                       noCheck
                       (blindRun cr ([mntPath, binPath cfg] <> selfBootstrapArgs) "")
                       noAction
                       noAction

-- | Configures a baseimage. Operations are meant to be called from inside a chroot.
bootstrapConfig :: NBDSlot -> BaseImageConfig -> DevOp a -> DevOp a
bootstrapConfig slot cfg extraConfig= do
   let login = superUser cfg
   let keys = pubKeys cfg
   let g = group login
   let extraGroups = fmap fst $ (traverse group ["sudo"]) `inject` deb "sudo"
   let u = user login g extraGroups
   let sshSecretsDir = userDirectory ".ssh" u
   let extraPkgs = traverse deb [(dhcpClientPackageName . suite) cfg, (kernelPackageName . suite) cfg ]
   let extraFiles = do
         authKeyspath <- (\(DirectoryPresent dirpath) -> dirpath </> "authorized_keys") <$> sshSecretsDir
         _ <- fileContent authKeyspath (pure keys) `inject` sshSecretsDir
         _ <- fileContent "/etc/sudoers" (pure sudoers) `inject` deb "sudo"
         _ <- fileContent "/etc/fstab" (pure fstab)
         _ <- fileContent "/etc/dhcp/dhclient-exit-hooks.d/hostname" (pure hostnameDhcpHook) `inject` (deb $ (dhcpClientPackageName . suite) cfg)
         _ <- fileContent "/etc/network/interfaces.d/loopback" (pure loopbackIfaceConfig) `inject` deb "ifupdown"
         _ <- ethernetAdapterDchpConfig (suite cfg)
         return ()
   let baseConfig = grubSetup slot `inject` (u >> extraPkgs >> extraFiles)
   fst <$> (extraConfig `inject` baseConfig)

-- | Setups grub on the NBD device from the chroot and fixes grub.cfg
grubSetup :: NBDSlot -> DevOp ()
grubSetup slot = devop (const ()) mkOp $ do
  gi <- grubInstall
  ug <- updateGrub
  let path = nbdDevicePath slot
  let rootpath = nbdPartitionPath slot 2
  return (gi, ug, path, rootpath)
  where mkOp (gi,ug,path, rootpath) = buildOp ("setup-grub") ("dirty grub setup from a chroot")
                          (noCheck)
                          (blindRun gi ["--recheck", path] ""
                           >> blindRun ug [] ""
                           >> replaceInFile "/boot/grub/grub.cfg" (Text.pack rootpath) "/dev/sda2")
                          (noAction)
                          (noAction)

-- | Configuration content for loopback interface.
loopbackIfaceConfig :: FileContent
loopbackIfaceConfig = convertString $ unlines [
    "auto lo"
  , "iface lo inet loopback"
  ]

-- | Configuration content for DHCP eth0 interface.
eth0IfaceConfig :: FileContent
eth0IfaceConfig = convertString $ unlines [
    "auto eth0"
  , "iface eth0 inet dhcp"
  ]

ens3IfaceConfig :: FileContent
ens3IfaceConfig = convertString $ unlines [
    "auto ens3"
  , "iface ens3 inet dhcp"
  ]

-- | Configuration content for the fstab.
-- TODO: build from a partition schema instead.
fstab :: FileContent
fstab = convertString $ unlines [
    "/dev/sda2 /    ext3 errors=remount-ro 0 1"
  , "/dev/sda1 none swap sw                0 0"
  ]

-- | Configuration hook to receive the hostname from DHCP.
hostnameDhcpHook :: FileContent
hostnameDhcpHook = "hostname $new_host_name"

-- | Configuration file to let sudo-group members run commands without a
-- password.
sudoers :: FileContent
sudoers = convertString $ unlines [ "Defaults env_reset"
                  , "Defaults mail_badpass"
                  , "Defaults secure_path=\"/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\""
                  , "root ALL=(ALL:ALL) ALL"
                  , "%admin ALL=(ALL) ALL"
                  , "%sudo   ALL=(ALL) NOPASSWD:ALL"
                  ]
