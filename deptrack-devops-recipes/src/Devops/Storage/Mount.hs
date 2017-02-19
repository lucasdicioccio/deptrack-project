{-# LANGUAGE OverloadedStrings #-}

module Devops.Storage.Mount where

import           Data.Monoid             ((<>))
import qualified Data.Text               as Text

import qualified Devops.Debian.Commands as Cmd
import           Devops.Base
import           Devops.Storage
import           Devops.Storage.Format hiding (partition)
import           Devops.Utils

data MountedPartition = MountedPartition {
    mountPoint       :: FilePath
  , mountedPartition :: NamedPartition
  }

mount :: DevOp NamedPartition
      -> DevOp DirectoryPresent
      -> DevOp MountedPartition
mount mkpart mkdir = devop fst mkOp $ do
    part <- mkpart
    (DirectoryPresent mntdir) <- mkdir
    mnt  <- Cmd.mount
    umnt <- Cmd.umount
    return $ (MountedPartition mntdir part, (mnt, umnt))
  where
    mkOp (MountedPartition mntdir (_, devpath), (mnt, umnt)) =
        buildOp ("mount: " <> Text.pack mntdir)
                ("Mounts" <> Text.pack devpath <> " on " <> Text.pack mntdir)
                noCheck
                (blindRun mnt [devpath, mntdir] "")
                (blindRun umnt [devpath] "")
                noAction
