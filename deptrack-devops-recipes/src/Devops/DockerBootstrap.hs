{-# LANGUAGE OverloadedStrings #-}

module Devops.DockerBootstrap (
    DockerBase
  , simpleBootstrap
  , xenial
  ) where

import           Devops.Base
import           Devops.BaseImage
import           Devops.Callback
import           Devops.Debootstrap (DebootstrapSuite)
import qualified Devops.Debootstrap as Debootstrap
import           Devops.Storage

type DockerBase = ()

xenial :: DebootstrapSuite DockerBase
xenial = Debootstrap.xenial ()

simpleBootstrap :: FilePath
                -> (BaseImageConfig DockerBase)
                -> CallBackMethod
                -> DevOp (BaseImage DockerBase)
simpleBootstrap imgpath cfg cb = do
    bootstrap imgpath (directory imgpath) cfg cb
