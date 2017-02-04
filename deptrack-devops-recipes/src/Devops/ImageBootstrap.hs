{-# LANGUAGE OverloadedStrings #-}

module Devops.ImageBootstrap where

import           Control.Monad    (void)

import           Devops.BaseImage
import           Devops.Callback
import           Devops.QemuNbd
import           Devops.Base

data BootstrapPhase = Setup | Configure

data BootStrapSetupConfig = BootStrapSetupConfig {
    nbdSlot         :: !NBDSlot
  , baseImageConfig :: !BaseImageConfig
  , configCallBack  :: !CallBackMethod
  }

noExtra :: DevOp ()
noExtra = pure ()

bootstrapPhase :: DevOp () -> BootstrapPhase -> BootStrapSetupConfig -> DevOp ()
bootstrapPhase _     Setup     cfg = void $ unpack cfg
bootstrapPhase extra Configure cfg = configure cfg extra

unpack :: BootStrapSetupConfig -> DevOp (BaseImage)
unpack cfg =
  let callback = configCallBack cfg
      baseimg  = baseImageConfig cfg
      slot     = nbdSlot cfg
  in bootstrap "debootstrap-dir" "./base-image.qcow2" slot baseimg callback

configure :: BootStrapSetupConfig -> DevOp () -> DevOp ()
configure cfg extra =
  let baseimg  = baseImageConfig cfg
      slot     = nbdSlot cfg
  in bootstrapConfig slot baseimg extra
