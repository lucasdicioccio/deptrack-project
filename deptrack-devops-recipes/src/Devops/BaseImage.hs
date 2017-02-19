{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Devops.BaseImage where

import           Data.Monoid             ((<>))
import           Data.String.Conversions (convertString)
import qualified Data.Text               as Text
import           DepTrack                (declare, inject)
import           System.FilePath         (makeRelative, (</>))

import           Devops.Base
import           Devops.Callback
import           Devops.Debian
import qualified Devops.Debian.Commands as Cmd
import           Devops.Debian.User
import           Devops.Debootstrap
import           Devops.QemuNbd
import           Devops.Storage
import           Devops.Storage.Format
import           Devops.Storage.Mount
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
  , cfgSuite  :: !DebootstrapSuite
  }

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
    let schema = Schema [ Partition 0   512   LinuxSwap
                        , Partition 512 20000 Ext3
                        ]
    let nbd = nbdMount slot qcow
    let nbdBlock = fmap nbdDevice nbd
    let formatted = formatDevice (partition schema nbdBlock)
    let rootPartition = fmap (head . namedPartitions . unFormat) formatted
    let base = debootstrapped (cfgSuite cfg) dirname (mount rootPartition (return dirname))
    let makeDestPath (Debootstrapped (DirectoryPresent x)) = x </> (makeRelative "/" (binPath cfg))
    let desc1 = "copies " <> Text.pack selfPath <> " in config chroot for " <> Text.pack imgpath
    let copy = declare (noop "ready-to-configure" desc1) $ do
                 fileCopy (makeDestPath <$> base) src
    let main = devop fst mkOp $ do
          (Debootstrapped (DirectoryPresent mntPath)) <- base
          _ <- copy
          chroot <- Cmd.chroot
          return (BaseImage (FilePresent imgpath) cfg, (chroot,mntPath))
    fmap snd (backedupQcow `inject` main)
  where
    mkOp (_,(chroot,mntPath)) = buildOp
        ("bootstrap-configured") ("finalizes configuration")
        noCheck
        (blindRun chroot ([mntPath, binPath cfg] <> selfBootstrapArgs) "")
        noAction
        noAction
bootstrapWithStore _ _ _ _ _ NoCallBack = error "TODO: cannot bootstrap with no callback"

-- | Configures a baseimage. Operations are meant to be called from inside a chroot.
bootstrapConfig :: NBDSlot -> BaseImageConfig -> DevOp a -> DevOp a
bootstrapConfig slot cfg extraConfig= do
     let login = superUser cfg
     let keys = pubKeys cfg
     let g = group login
     let extraGroups = fmap fst $ (traverse group ["sudo"]) `inject` deb "sudo"
     let u = user login g extraGroups
     let sshSecretsDir = userDirectory ".ssh" u
     let extraPkgs = traverse deb [(dhcpClientPackageName . cfgSuite) cfg, (kernelPackageName . cfgSuite) cfg ]
     let extraFiles = do
           authKeyspath <- (\(DirectoryPresent dirpath) -> dirpath </> "authorized_keys") <$> sshSecretsDir
           _ <- fileContent authKeyspath (pure keys) `inject` sshSecretsDir
           _ <- fileContent "/etc/sudoers" (pure sudoers) `inject` deb "sudo"
           _ <- fileContent "/etc/fstab" (pure fstab)
           _ <- fileContent "/etc/dhcp/dhclient-exit-hooks.d/hostname" (pure hostnameDhcpHook) `inject` (deb $ (dhcpClientPackageName . cfgSuite) cfg)
           _ <- fileContent "/etc/network/interfaces.d/loopback" (pure loopbackIfaceConfig) `inject` deb "ifupdown"
           _ <- ethernetAdapterDchpConfig (cfgSuite cfg)
           return ()
     let baseConfig = grubSetup slot `inject` (u >> extraPkgs >> extraFiles)
     fst <$> (extraConfig `inject` baseConfig)

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
