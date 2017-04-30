{-# LANGUAGE OverloadedStrings #-}
module Devops.Parasite (
    ParasitedHost (..)
  , ParasiteLogin
  , ControlledHost (..)
  , control
  , parasite
  , remoted
  , fileTransferred
  , SshFsMountedDir , sshMounted , sshFileCopy
  , FileTransferred
  ) where

import           Data.Monoid                 ((<>))
import           Data.String.Conversions     (convertString)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Data.Typeable               (Typeable)
import           System.FilePath.Posix       (takeBaseName, (</>))

import           DepTrack
import           Devops.Callback
import           Devops.Cli
import           Devops.Debian.Commands      hiding (r, umount)
import           Devops.Debian.User          (homeDirPath)
import           Devops.Networking
import           Devops.Storage
import           Devops.Base
import           Devops.Utils

type ParasiteLogin = Text

-- | A host that we control.
data ControlledHost = ControlledHost !ParasiteLogin !Remote

-- | A host that we control.
data ParasitedHost = ParasitedHost !FilePath !ParasiteLogin !IpNetString

-- | A file transferred at a given remote path.
data FileTransferred = FileTransferred !FilePath !Remote

-- | Assert control on a remote.
control :: ParasiteLogin -> DevOp Remote -> DevOp ControlledHost
control login mkRemote = devop fst mkOp $ do
    r@(Remote ip) <- mkRemote
    return ((ControlledHost login r), ip)
  where
    mkOp (ControlledHost _ _, ip) = buildOp ("controlled-host: " <> ip)
                                            ("declares a host is controllable")
                                            noCheck
                                            noAction
                                            noAction
                                            noAction

-- | A parasite reserves a binary in the homedir.
--
-- The remote binary will take precedence in 'BinaryCall' from 'remoted'
-- 'Continued' argument.
parasite :: FilePath -> DevOp ControlledHost -> DevOp ParasitedHost
parasite selfPath mkHost = track mkOp $ do
    (ControlledHost login _) <- mkHost
    let selfBinary = preExistingFile selfPath
    let rpath = homeDirPath login </> takeBaseName selfPath
    (FileTransferred _ (Remote ip)) <- fileTransferred selfBinary rpath mkHost
    return (ParasitedHost rpath login ip)
  where
    mkOp (ParasitedHost _ _ ip) = noop ("parasited-host: " <> ip)
                                       ("copies itself after in a parasite")

-- | Turnup a given DevOp at a given remote.
remoted :: Typeable a
        => Continued a
        -> DevOp ParasitedHost
        -> DevOp (Remoted a)
remoted cont host = devop fst mkOp $ do
    c <- ssh
    let obj = eval cont
    (BinaryCall _ fArgs) <- callback cont
    let args = fArgs TurnUp
    (ParasitedHost rpath login ip) <- host
    return ((Remoted (Remote ip) obj), (rpath, login, c, args, ip))
  where
    mkOp (_, (rpath, login, c, args, ip)) =
        buildOp
            ("remote-callback: " <> convertString (unwords args) <> " @" <> ip)
            ("calls itself back with `$self " <> convertString (unwords args) <>"`")
            noCheck
            (blindRun c (sshCmd rpath login ip args) "")
            noAction
            noAction
    sshCmd rpath login ip args = [
        "-o", "StrictHostKeyChecking no"
      , "-o", "UserKnownHostsFile /dev/null"
      , "-l", Text.unpack login, Text.unpack ip
      , "sudo", "-E", rpath
      ] ++ args

-- | A file transferred at a remote path.
fileTransferred :: DevOp FilePresent
                -> FilePath
                -> DevOp ControlledHost
                -> DevOp (FileTransferred)
fileTransferred mkFp path mkHost = devop fst mkOp $ do
    c <- scp
    f <- mkFp
    (ControlledHost login r) <- mkHost
    return (FileTransferred path r, (f,c,login))
  where
    mkOp (FileTransferred rpath (Remote ip), (FilePresent lpath,c,login)) = do
            buildOp
                ("remote-file: " <> Text.pack rpath <> "@" <> ip)
                ("file " <> Text.pack lpath <> " copied on " <> ip)
                noCheck
                (blindRun c (scpcmd lpath login ip rpath) "")
                noAction
                noAction
    scpcmd lpath login ip rpath = [
          "-o", "StrictHostKeyChecking no"
        , "-o", "UserKnownHostsFile /dev/null"
        , lpath
        , Text.unpack login ++ "@" ++ Text.unpack ip ++ ":" ++ rpath
        ]

-- Remote storage mounting.
data SshFsMountedDir = SshFsMountedDir !FilePath

sshMounted :: DevOp DirectoryPresent -> DevOp ControlledHost -> DevOp (SshFsMountedDir)
sshMounted mkPath mkHost = devop fst mkOp $ do
    binmount <- mount
    sshmount <- sshfs
    umount <- fusermount
    (DirectoryPresent path) <- mkPath
    host <- mkHost
    return (SshFsMountedDir path, (host, binmount, sshmount, umount))
  where
    mkOp (SshFsMountedDir path, (host, binmount, sshmount, umount)) = do
        let (ControlledHost login (Remote ip)) = host
        buildOp ("ssh-fs-dir: " <> Text.pack path <> "@" <> ip)
                ("mount " <> ip <> " at mountpoint " <> Text.pack path)
                (checkBinaryExitCodeAndStdout (hasMountLine path)
                                               binmount
                                               ["-l", "-t", "fuse.sshfs"] "")
                (blindRun sshmount [ Text.unpack login ++ "@" ++ Text.unpack ip ++ ":"
                                   , path
                                   , "-o", "StrictHostKeyChecking=no"
                                   , "-o", "UserKnownHostsFile=/dev/null"
                                   ] "")
                (blindRun umount [ "-u", path ] "")
                noAction
    -- | Looks for the filepath in the list of mounts.
    hasMountLine :: FilePath -> String -> Bool
    hasMountLine path dat = elem path $ concatMap words $ lines dat

sshFileCopy :: DevOp FilePresent -> DevOp (SshFsMountedDir) -> DevOp (RepositoryFile, FilePresent)
sshFileCopy mkLocal mkDir = do
    (FilePresent loc) <- mkLocal
    let rpath = (\(SshFsMountedDir dir) -> dir </> takeBaseName loc) <$> mkDir
    fileCopy rpath (mkLocal >>= (\(FilePresent local) -> localRepositoryFile local))
