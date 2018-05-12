{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Devops.Debian.Packages where

import           Devops.Binary
import           Devops.Debian.Base
import           Devops.Debian.Repositories
import qualified Devops.Debian.Repositories as Repos
import           Devops.Base

apparmor :: DevOp env (DebianPackage "apparmor")
apparmor = debianPackage

bind9 :: DevOp env (DebianPackage "bind9")
bind9 = debianPackage

bridgeUtils :: DevOp env (DebianPackage "bridge-utils")
bridgeUtils = debianPackage

coreutils :: DevOp env (DebianPackage "coreutils")
coreutils = debianPackage

debootstrap :: DevOp env (DebianPackage "debootstrap")
debootstrap = debianPackage

dockerEngine :: DevOp env (DebianPackage "docker-engine")
dockerEngine = generalDebianPackage "docker-engine" Repos.docker

dotnetCore :: DevOp env (DebianPackage "dotnet-dev")
dotnetCore = generalDebianPackage "dotnet-dev-1.0.1" Repos.dotnet

e2fsprogs :: DevOp env (DebianPackage "e2fsprogs")
e2fsprogs = debianPackage

fuse :: DevOp env (DebianPackage "fuse")
fuse = debianPackage

gitCore :: DevOp env (DebianPackage "git-core")
gitCore = debianPackage

grub :: DevOp env (DebianPackage "grub-pc")
grub = debianPackage

iproute2 :: DevOp env (DebianPackage "iproute2")
iproute2 = debianPackage

iptables :: DevOp env (DebianPackage "iptables")
iptables = debianPackage

iscDhcpServer :: DevOp env (DebianPackage "isc-dhcp-server")
iscDhcpServer = debianPackage

jenkins :: DevOp env (DebianPackage "jenkins")
jenkins = generalDebianPackage "jenkins" Repos.jenkins

kmod :: DevOp env (DebianPackage "kmod")
kmod = debianPackage

mount :: DevOp env (DebianPackage "mount")
mount = debianPackage

nginx :: DevOp env (DebianPackage "nginx")
nginx = debianPackage

opensshClient :: DevOp env (DebianPackage "openssh-client")
opensshClient = debianPackage

parted :: DevOp env (DebianPackage "parted")
parted = debianPackage

passwd :: DevOp env (DebianPackage "passwd")
passwd = debianPackage

python3Pip :: DevOp env (DebianPackage "python3-pip")
python3Pip = debianPackage

qemu :: DevOp env (DebianPackage "qemu")
qemu = debianPackage

qemuUtils :: DevOp env (DebianPackage "qemu-utils")
qemuUtils = debianPackage

rBaseDev :: DevOp env (DebianPackage "r-base-dev")
rBaseDev = generalDebianPackage "r-base-dev" rCran

sshfs :: DevOp env (DebianPackage "sshfs")
sshfs = debianPackage

stack :: DevOp env (DebianPackage "stack")
stack = generalDebianPackage "stack" Repos.fpComplete

umlUtilities :: DevOp env (DebianPackage "uml-utilities")
umlUtilities = debianPackage

utilLinux :: DevOp env (DebianPackage "util-linux")
utilLinux = debianPackage

wget :: DevOp env (DebianPackage "wget")
wget = debianPackage

tar :: DevOp env (DebianPackage "tar")
tar = debianPackage

instance HasBinary (DebianPackage "apparmor") "apparmor_parser"
instance HasBinary (DebianPackage "bind9") "/usr/sbin/named"
instance HasBinary (DebianPackage "bridge-utils") "brctl"
instance HasBinary (DebianPackage "coreutils") "chroot"
instance HasBinary (DebianPackage "coreutils") "cp"
instance HasBinary (DebianPackage "coreutils") "sync"
instance HasBinary (DebianPackage "debootstrap") "debootstrap"
instance HasBinary (DebianPackage "docker-engine") "docker"
instance HasBinary (DebianPackage "e2fsprogs") "mkfs.ext3"
instance HasBinary (DebianPackage "fuse") "fusermount"
instance HasBinary (DebianPackage "git-core") "git"
instance HasBinary (DebianPackage "grub-pc") "grub-install"
instance HasBinary (DebianPackage "grub-pc") "update-grub"
instance HasBinary (DebianPackage "iproute2") "ip"
instance HasBinary (DebianPackage "iptables") "iptables"
instance HasBinary (DebianPackage "kmod") "modprobe"
instance HasBinary (DebianPackage "mount") "mount"
instance HasBinary (DebianPackage "mount") "umount"
instance HasBinary (DebianPackage "nginx") "/usr/sbin/nginx"
instance HasBinary (DebianPackage "openssh-client") "scp"
instance HasBinary (DebianPackage "openssh-client") "ssh"
instance HasBinary (DebianPackage "openssh-client") "ssh-keygen"
instance HasBinary (DebianPackage "parted") "parted"
instance HasBinary (DebianPackage "passwd") "groupadd"
instance HasBinary (DebianPackage "passwd") "groupdel"
instance HasBinary (DebianPackage "passwd") "useradd"
instance HasBinary (DebianPackage "passwd") "userdel"
instance HasBinary (DebianPackage "python3-pip") "pip3"
instance HasBinary (DebianPackage "qemu") "/usr/bin/qemu-system-x86_64"
instance HasBinary (DebianPackage "qemu-utils") "qemu-img"
instance HasBinary (DebianPackage "qemu-utils") "qemu-nbd"
instance HasBinary (DebianPackage "r-base-dev") "R"
instance HasBinary (DebianPackage "sshfs") "sshfs"
instance HasBinary (DebianPackage "stack") "stack"
instance HasBinary (DebianPackage "uml-utilities") "tunctl"
instance HasBinary (DebianPackage "util-linux") "mkswap"
instance HasBinary (DebianPackage "util-linux") "sfdisk"
instance HasBinary (DebianPackage "wget") "wget"
instance HasBinary (DebianPackage "tar") "tar"
instance HasBinary (DebianPackage "isc-dhcp-server") "/usr/sbin/dhcpd"
