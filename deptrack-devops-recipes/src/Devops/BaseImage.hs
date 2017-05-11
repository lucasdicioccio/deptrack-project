{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

-- | Module representing base images in an immutable-infrastructure approach.
--
-- This module proposes to bootstrap base images locally from a debootstrapped
-- environment and using a chroot.
module Devops.BaseImage where

import           Data.Monoid             ((<>))
import qualified Data.Text               as Text
import           Data.Typeable           (Typeable)
import           DepTrack                (declare)
import           System.FilePath         (makeRelative, (</>))

import           Devops.Base
import           Devops.Callback
import           Devops.Cli
import qualified Devops.Debian.Commands as Cmd
import           Devops.Debootstrap
import           Devops.Storage
import           Devops.Utils

data BaseImage a = BaseImage {
    imagePath :: !FilePresent
  , config    :: !(BaseImageConfig a)
  }
data BaseImageConfig a = BaseImageConfig {
    binPath   :: !FilePath -- Path to binary to turnup a new base image.
  , cfgSuite  :: !(DebootstrapSuite a)
  }

-- TODO: refactor to take DevOp BaseImageConfig and DevOp BinaryCall
bootstrap :: Typeable a
          => FilePath                 -- Path receiving the image.
          -> DevOp DirectoryPresent   -- Directory receiving the debootstrap environment.
          -> (BaseImageConfig a)      -- Configuration of the base image.
          -> DeboostrapMounts         -- What to mount in the bootstrapped environment.
          -> BinaryCall
          -> DevOp (BaseImage a)
bootstrap imgpath bootstrapdir cfg mounts cb = devop fst mkOp $ do
    let src = localRepositoryFile selfPath
    let base = debootstrapped (cfgSuite cfg) mounts bootstrapdir
    let makeDestPath (Debootstrapped (DirectoryPresent x)) = x </> (makeRelative "/" (binPath cfg))
    let desc1 = "copies " <> Text.pack selfPath <> " in debootstrapped dir for " <> Text.pack imgpath
    (Debootstrapped (DirectoryPresent mntPath)) <- base
    _ <- declare (noop "ready-to-configure" desc1) $ do
        fileCopy (makeDestPath <$> base) src
    chroot <- Cmd.chroot
    return (BaseImage (FilePresent imgpath) cfg, (chroot, mntPath))
  where
    (BinaryCall selfPath selfBootstrapArgs) = cb
    mkOp (_,(chroot,mntPath)) = buildOp
        ("bootstrap-configured") ("finalizes configuration")
        noCheck
        (blindRun chroot ([mntPath, binPath cfg] <> selfBootstrapArgs (TurnUp Concurrently)) "")
        noAction
        noAction
