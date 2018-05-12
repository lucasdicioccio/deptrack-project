{-# LANGUAGE DataKinds #-}

module  Devops.Debian.Commands where

import           Devops.Binary          (Binary, binary, bin)
import           Devops.Debian.Base     (installedWith)
import qualified Devops.Debian.Packages as Pkg
import           Devops.Base           (DevOp)

apparmorParser :: DevOp env (Binary "apparmor_parser")
apparmorParser = binary `installedWith` Pkg.apparmor

cp :: DevOp env (Binary "cp")
cp = binary `installedWith` Pkg.coreutils

qemux86 :: DevOp env (Binary "/usr/bin/qemu-system-x86_64")
qemux86 = binary `installedWith` Pkg.qemu

qemuImg :: DevOp env (Binary "qemu-img")
qemuImg = binary `installedWith` Pkg.qemuUtils

qemuNbd :: DevOp env (Binary "qemu-nbd")
qemuNbd = binary `installedWith` Pkg.qemuUtils

bind9 :: DevOp env (Binary "/usr/sbin/named")
bind9 = binary `installedWith` Pkg.bind9

chroot :: DevOp env (Binary "chroot")
chroot = binary `installedWith` Pkg.coreutils

debootstrap :: DevOp env (Binary "debootstrap")
debootstrap = binary `installedWith` Pkg.debootstrap

docker :: DevOp env (Binary "docker")
docker = binary `installedWith` Pkg.dockerEngine

dotnet :: DevOp env (Binary "dotnet")
dotnet = pure (bin "/usr/bin/dotnet") `installedWith` Pkg.dotnetCore

dhcpd :: DevOp env (Binary "/usr/sbin/dhcpd")
dhcpd = binary `installedWith` Pkg.iscDhcpServer

mkswap :: DevOp env (Binary "mkswap")
mkswap = binary `installedWith` Pkg.utilLinux

mount :: DevOp env (Binary "mount")
mount = binary `installedWith` Pkg.mount

umount :: DevOp env (Binary "umount")
umount = binary `installedWith` Pkg.mount

modprobe :: DevOp env (Binary "modprobe")
modprobe = binary `installedWith` Pkg.kmod

mkfsExt3 :: DevOp env (Binary "mkfs.ext3")
mkfsExt3 = binary `installedWith` Pkg.e2fsprogs

grubInstall :: DevOp env (Binary "grub-install")
grubInstall = binary `installedWith` Pkg.grub

updateGrub :: DevOp env (Binary "update-grub")
updateGrub = binary `installedWith` Pkg.grub

nginx :: DevOp env (Binary "/usr/sbin/nginx")
nginx = binary `installedWith` Pkg.nginx

parted :: DevOp env (Binary "/sbin/parted")
parted = binary `installedWith` Pkg.parted

sshfs :: DevOp env (Binary "sshfs")
sshfs = binary `installedWith` Pkg.sshfs

sfdisk :: DevOp env (Binary "sfdisk")
sfdisk = binary `installedWith` Pkg.utilLinux

ssh :: DevOp env (Binary "ssh")
ssh = binary `installedWith` Pkg.opensshClient

sshKeygen :: DevOp env (Binary "ssh-keygen")
sshKeygen = binary `installedWith` Pkg.opensshClient

scp :: DevOp env (Binary "scp")
scp = binary `installedWith` Pkg.opensshClient

fusermount :: DevOp env (Binary "fusermount")
fusermount = binary `installedWith` Pkg.fuse

binIp :: DevOp env (Binary "ip")
binIp = binary `installedWith` Pkg.iproute2

brctl :: DevOp env (Binary "brctl")
brctl = binary `installedWith` Pkg.bridgeUtils

tunctl :: DevOp env (Binary "tunctl")
tunctl = binary `installedWith` Pkg.umlUtilities

iptables :: DevOp env (Binary "iptables")
iptables = binary `installedWith` Pkg.iptables

useradd :: DevOp env (Binary "useradd")
useradd = binary `installedWith` Pkg.passwd

userdel :: DevOp env (Binary "userdel")
userdel = binary `installedWith` Pkg.passwd

groupadd :: DevOp env (Binary "groupadd")
groupadd = binary `installedWith` Pkg.passwd

groupdel :: DevOp env (Binary "groupdel")
groupdel = binary `installedWith` Pkg.passwd

git :: DevOp env (Binary "git")
git = binary `installedWith` Pkg.gitCore

pip3 :: DevOp env (Binary "pip3")
pip3 = binary `installedWith` Pkg.python3Pip

r :: DevOp env (Binary "R")
r = binary `installedWith` Pkg.rBaseDev

stack :: DevOp env (Binary "stack")
stack = binary `installedWith` Pkg.stack

sync :: DevOp env (Binary "sync")
sync = binary `installedWith` Pkg.coreutils

wget :: DevOp env (Binary "wget")
wget = binary `installedWith` Pkg.wget

tar :: DevOp env (Binary "tar")
tar = binary `installedWith` Pkg.tar
