{-# LANGUAGE OverloadedStrings #-}

module Devops.Docker (
  ) where

import           Devops.Base
import           Devops.BaseImage
import           Devops.DockerBootstrap

data DockerImage = DockerImage Name

dockerImage :: Name -> DevOp (BaseImage DockerBase) -> DevOp (DockerImage)
dockerImage name mkBase = devop _ _ $ do
    base <- mkBase

