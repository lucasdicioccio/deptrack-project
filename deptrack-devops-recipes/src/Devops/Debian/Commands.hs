{-# LANGUAGE DataKinds #-}

module  Devops.Debian.Commands where

import           Devops.Binary          (Binary, binary, bin)
import           Devops.Debian.Base     (installedWith)
import qualified Devops.Debian.Packages as Pkg
import           Devops.Base           (DevOp)

apparmorParser :: DevOp (Binary "apparmor_parser")
apparmorParser = binary `installedWith` Pkg.apparmor

cp :: DevOp (Binary "cp")
cp = binary `installedWith` Pkg.coreutils

qemux86 :: DevOp (Binary "/usr/bin/qemu-system-x86_64")
qemux86 = binary `installedWith` Pkg.qemu

qemuImg :: DevOp (Binary "qemu-img")
qemuImg = binary `installedWith` Pkg.qemuUtils

qemuNbd :: DevOp (Binary "qemu-nbd")
qemuNbd = binary `installedWith` Pkg.qemuUtils

bind9 :: DevOp (Binary "/usr/sbin/named")
bind9 = binary `installedWith` Pkg.bind9

chroot :: DevOp (Binary "chroot")
chroot = binary `installedWith` Pkg.coreutils

debootstrap :: DevOp (Binary "debootstrap")
debootstrap = binary `installedWith` Pkg.debootstrap

docker :: DevOp (Binary "docker")
docker = binary `installedWith` Pkg.dockerEngine

dotnet :: DevOp (Binary "dotnet")
dotnet = pure (bin "/usr/bin/dotnet") `installedWith` Pkg.dotnetCore

dhcpd :: DevOp (Binary "/usr/sbin/dhcpd")
dhcpd = binary `installedWith` Pkg.iscDhcpServer

mkswap :: DevOp (Binary "mkswap")
mkswap = binary `installedWith` Pkg.utilLinux

mount :: DevOp (Binary "mount")
mount = binary `installedWith` Pkg.mount

umount :: DevOp (Binary "umount")
umount = binary `installedWith` Pkg.mount

modprobe :: DevOp (Binary "modprobe")
modprobe = binary `installedWith` Pkg.kmod

mkfsExt3 :: DevOp (Binary "mkfs.ext3")
mkfsExt3 = binary `installedWith` Pkg.e2fsprogs

grubInstall :: DevOp (Binary "grub-install")
grubInstall = binary `installedWith` Pkg.grub

updateGrub :: DevOp (Binary "update-grub")
updateGrub = binary `installedWith` Pkg.grub

nginx :: DevOp (Binary "/usr/sbin/nginx")
nginx = binary `installedWith` Pkg.nginx

parted :: DevOp (Binary "/sbin/parted")
parted = binary `installedWith` Pkg.parted

sshfs :: DevOp (Binary "sshfs")
sshfs = binary `installedWith` Pkg.sshfs

sfdisk :: DevOp (Binary "sfdisk")
sfdisk = binary `installedWith` Pkg.utilLinux

ssh :: DevOp (Binary "ssh")
ssh = binary `installedWith` Pkg.opensshClient

sshKeygen :: DevOp (Binary "ssh-keygen")
sshKeygen = binary `installedWith` Pkg.opensshClient

scp :: DevOp (Binary "scp")
scp = binary `installedWith` Pkg.opensshClient

fusermount :: DevOp (Binary "fusermount")
fusermount = binary `installedWith` Pkg.fuse

binIp :: DevOp (Binary "ip")
binIp = binary `installedWith` Pkg.iproute2

brctl :: DevOp (Binary "brctl")
brctl = binary `installedWith` Pkg.bridgeUtils

tunctl :: DevOp (Binary "tunctl")
tunctl = binary `installedWith` Pkg.umlUtilities

iptables :: DevOp (Binary "iptables")
iptables = binary `installedWith` Pkg.iptables

useradd :: DevOp (Binary "useradd")
useradd = binary `installedWith` Pkg.passwd

userdel :: DevOp (Binary "userdel")
userdel = binary `installedWith` Pkg.passwd

groupadd :: DevOp (Binary "groupadd")
groupadd = binary `installedWith` Pkg.passwd

groupdel :: DevOp (Binary "groupdel")
groupdel = binary `installedWith` Pkg.passwd

git :: DevOp (Binary "git")
git = binary `installedWith` Pkg.gitCore

pip3 :: DevOp (Binary "pip3")
pip3 = binary `installedWith` Pkg.python3Pip

r :: DevOp (Binary "R")
r = binary `installedWith` Pkg.rBaseDev

stack :: DevOp (Binary "stack")
stack = binary `installedWith` Pkg.stack

sync :: DevOp (Binary "sync")
sync = binary `installedWith` Pkg.coreutils

wget :: DevOp (Binary "wget")
wget = binary `installedWith` Pkg.wget

tar :: DevOp (Binary "tar")
tar = binary `installedWith` Pkg.tar
