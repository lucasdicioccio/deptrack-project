{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Devops.BaseImage where

import           Data.Monoid             ((<>))
import qualified Data.Text               as Text
import           Data.Typeable           (Typeable)
import           DepTrack                (declare)
import           System.FilePath         (makeRelative, (</>))

import           Devops.Base
import           Devops.Callback
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

-- TODO: refactor to take DevOp BaseImageConfig and DevOp CallBackMethod
bootstrap :: Typeable a
          => FilePath                 -- Path receiving the qemu image.
          -> DevOp DirectoryPresent   -- directory receiving the debootstrap environment
          -> (BaseImageConfig a)      -- Configuration of the base image.
          -> CallBackMethod
          -> DevOp (BaseImage a)
bootstrap imgpath bootstrapdir cfg (BinaryCall selfPath selfBootstrapArgs) = devop fst mkOp $ do
    let src = localRepositoryFile selfPath
    let base = debootstrapped (cfgSuite cfg) bootstrapdir

    let makeDestPath (Debootstrapped (DirectoryPresent x)) = x </> (makeRelative "/" (binPath cfg))
    let desc1 = "copies " <> Text.pack selfPath <> " in debootstrapped dir for " <> Text.pack imgpath
    (Debootstrapped (DirectoryPresent mntPath)) <- base
    _ <- declare (noop "ready-to-configure" desc1) $ do
        fileCopy (makeDestPath <$> base) src
    chroot <- Cmd.chroot
    return (BaseImage (FilePresent imgpath) cfg, (chroot, mntPath))
  where
    mkOp (_,(chroot,mntPath)) = buildOp
        ("bootstrap-configured") ("finalizes configuration")
        noCheck
        (blindRun chroot ([mntPath, binPath cfg] <> selfBootstrapArgs) "")
        noAction
        noAction
