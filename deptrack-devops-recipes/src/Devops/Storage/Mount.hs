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
    mountPoint       :: DirectoryPresent
  , mountedPartition :: NamedPartition
  }

mount :: DevOp env NamedPartition
      -> DevOp env DirectoryPresent
      -> DevOp env MountedPartition
mount mkpart mkdir = devop fst mkOp $ do
    part <- mkpart
    dir@(DirectoryPresent mntdir) <- mkdir
    mnt  <- Cmd.mount
    umnt <- Cmd.umount
    return $ (MountedPartition dir part, (mntdir, mnt, umnt))
  where
    mkOp (MountedPartition _ (_, devpath), (mntdir, mnt, umnt)) =
        buildOp ("mount: " <> Text.pack mntdir)
                ("Mounts" <> Text.pack devpath <> " on " <> Text.pack mntdir)
                noCheck
                (blindRun mnt [devpath, mntdir] "")
                (blindRun umnt [devpath] "")
                noAction
