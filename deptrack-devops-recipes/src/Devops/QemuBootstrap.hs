{-# LANGUAGE OverloadedStrings #-}

module Devops.QemuBootstrap where

import           Data.Monoid             ((<>))
import           Data.String.Conversions (convertString)
import qualified Data.Text               as Text
import           DepTrack                (inject)
import           System.FilePath         ((</>))

import           Devops.Base
import           Devops.BaseImage
import           Devops.Callback
import           Devops.Debian
import qualified Devops.Debian.Commands as Cmd
import           Devops.Debian.User
import           Devops.Debootstrap
import qualified Devops.Debootstrap as Debootstrap
import           Devops.Storage.Format
import           Devops.Storage.Mount
import           Devops.QemuNbd
import           Devops.Storage
import           Devops.Utils

data QemuBase =
    QemuBase { kernelPackageName         :: !Name
             , dhcpClientPackageName     :: !Name
             , ethernetAdapterDchpConfig :: !(DevOp FilePresent)
             }

-- | Bootstraps a base image, copying the image after turndown.
--
-- This method is safer than dirtyBootstrap because it will force users to
-- release all temporary resources (i.e., after a full-cycle for
-- turnup/turndown).
synchedBootstrap :: FilePath  -- Path to a temporary dir receiving the debootstrap environment.
          -> FilePath  -- Path receiving the qemu image.
          -> NBDSlot   -- NBD device number to use during the bootstrap.
          -> (BaseImageConfig QemuBase) -- Configuration of the base image.
          -> GB Size
          -> CallBackMethod
          -> DevOp (BaseImage QemuBase)
synchedBootstrap = bootstrapWithImageCopyFunction (\x y -> fmap snd (turndownfileBackup x y))

-- | Bootstraps a base image, copying the image after turnup.
--
-- This method is not really safe to build a base image because it will copy
-- the base image content with no guarantee that NBD flushed everything and
-- while its filesystem is mounted.
--
-- You should only use this method to impress your friends or when you know
-- what you're doing (the library author is in the former situation, not the
-- latter).
dirtyBootstrap :: FilePath  -- Path to a temporary dir receiving the debootstrap environment.
          -> FilePath  -- Path receiving the qemu image.
          -> NBDSlot   -- NBD device number to use during the bootstrap.
          -> (BaseImageConfig QemuBase)  -- Configuration of the base image.
          -> GB Size
          -> CallBackMethod
          -> DevOp (BaseImage QemuBase)
dirtyBootstrap = bootstrapWithImageCopyFunction (\x y -> fmap snd (turnupfileBackup x y))

bootstrapWithImageCopyFunction ::
             (FilePath -> DevOp FilePresent -> DevOp FilePresent) -- Specifies a way to copy the image (see usage in bootstrap resp. dirtyBootstrap)
          -> FilePath  -- Path to a temporary dir receiving the debootstrap environment.
          -> FilePath  -- Path receiving the qemu image.
          -> NBDSlot   -- NBD device number to use during the bootstrap.
          -> (BaseImageConfig QemuBase)  -- Configuration of the base image.
          -> GB Size
          -> CallBackMethod
          -> DevOp (BaseImage QemuBase)
bootstrapWithImageCopyFunction imgcopy dirname imgpath slot cfg size cb = do
    let qcow = qcow2Image (imgpath <> ".tmp") size
    let backedupQcow = QemuImage <$> imgcopy imgpath (fmap getImage qcow)
    let schema = Schema [ Partition 1   512   LinuxSwap
                        , Partition 512 20000 Ext3
                        ]
    let nbd = nbdMount slot qcow
    let nbdBlock = fmap nbdDevice nbd
    let formatted = formatDevice (partition schema nbdBlock)
    let rootPartition = fmap ((!! 1) . namedPartitions . unFormat) formatted
    let mountedRootPartition = mount rootPartition (directory dirname)
    let bootstrapdir = fmap mountPoint mountedRootPartition

    let boot = bootstrap imgpath bootstrapdir cfg cb
    fmap snd (backedupQcow `inject` boot)

-- | Setups grub on the NBD device from the chroot and fixes grub.cfg
grubSetup :: NBDSlot -> DevOp ()
grubSetup slot = devop (const ()) mkOp $ do
    grubInstal <- Cmd.grubInstall
    updateGrub <- Cmd.updateGrub
    let path = nbdDevicePath slot
    let rootpath = nbdPartitionPath slot 2
    return (grubInstal, updateGrub, path, rootpath)
  where
    mkOp (grubInstall,updateGrub,path, rootpath) =
        buildOp ("setup-grub")
                ("dirty grub setup from a chroot")
                (noCheck)
                (blindRun grubInstall ["--recheck", path] ""
                 >> blindRun updateGrub [] ""
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

-- | Configures a baseimage. Operations are meant to be called from inside a chroot.
nbdBootstrapConfig :: NBDSlot -> BaseImageConfig QemuBase -> DevOp a -> DevOp a
nbdBootstrapConfig slot cfg extraConfig = do
    let login = superUser cfg
    let keys = pubKeys cfg
    let g = group login
    let extraGroups = fmap fst $ (traverse group ["sudo"]) `inject` deb "sudo"
    let u = user login g extraGroups
    let sshSecretsDir = userDirectory ".ssh" u
    let configFiles = do
            authKeyspath <- (\(DirectoryPresent dirpath) -> dirpath </> "authorized_keys") <$> sshSecretsDir
            _ <- fileContent authKeyspath (pure keys) `inject` sshSecretsDir
            _ <- fileContent "/etc/sudoers" (pure sudoers) `inject` deb "sudo"
            _ <- fileContent "/etc/fstab" (pure fstab)
            _ <- fileContent "/etc/network/interfaces.d/loopback" (pure loopbackIfaceConfig) `inject` deb "ifupdown"
            _ <- fileContent "/etc/dhcp/dhclient-exit-hooks.d/hostname" (pure hostnameDhcpHook) `inject` (deb $ (dhcpClientPackageName . specificConfiguration . cfgSuite) cfg)
            _ <- ethernetAdapterDchpConfig (specificConfiguration $ cfgSuite cfg)
            _ <- grubSetup slot
            return ()
    let basePkgs = traverse deb [
              (dhcpClientPackageName . specificConfiguration . cfgSuite) cfg
            , (kernelPackageName . specificConfiguration . cfgSuite) cfg
            ]
    let imageCfg = configFiles `inject` basePkgs
    fst <$> (extraConfig `inject` imageCfg)

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
sudoers = convertString $ unlines [
    "Defaults env_reset"
 , "Defaults mail_badpass"
 , "Defaults secure_path=\"/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\""
 , "root ALL=(ALL:ALL) ALL"
 , "%admin ALL=(ALL) ALL"
 , "%sudo   ALL=(ALL) NOPASSWD:ALL"
 ]

trusty :: DebootstrapSuite QemuBase
trusty = Debootstrap.trusty base
  where
    base = QemuBase "linux-signed-image-generic-lts-trusty" "dhcp-client" eth0
    eth0 = fmap snd $ fileContent "/etc/network/interfaces.d/eth0" (pure eth0IfaceConfig)

xenial :: DebootstrapSuite QemuBase
xenial = Debootstrap.xenial base
  where
    base = QemuBase "linux-signed-image-generic-lts-xenial" "isc-dhcp-client" ens3
    ens3 = fmap snd $ fileContent "/etc/network/interfaces.d/ens3" (pure ens3IfaceConfig)

jessie :: DebootstrapSuite QemuBase
jessie = Debootstrap.jessie base
  where
    base = QemuBase "linux-image-amd64" "isc-dhcp-client" eth0
    eth0 = fmap snd $ fileContent "/etc/network/interfaces.d/eth0" (pure eth0IfaceConfig)
