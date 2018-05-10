{-# LANGUAGE OverloadedStrings #-}

module Devops.DockerBootstrap (
    DockerBase
  , simpleBootstrap
  , xenial
  ) where

import           Data.Monoid        ((<>))

import           Devops.Base
import           Devops.BaseImage
import           Devops.Callback
import           Devops.Debootstrap (DebootstrapSuite, noMounts)
import qualified Devops.Debootstrap as Debootstrap
import           Devops.Storage

type DockerBase = ()

xenial :: DebootstrapSuite DockerBase
xenial = Debootstrap.xenial ()

simpleBootstrap :: FilePath
                -> (BaseImageConfig DockerBase)
                -> BinaryCall
                -> DevOp (BaseImage DockerBase)
simpleBootstrap imgpath cfg cb = do
    let debootstrapdir = directory (imgpath <> ".debootstrapdir")
    let deboostrapping = bootstrap imgpath debootstrapdir cfg noMounts cb
    let archiving = tarDirectory imgpath debootstrapdir
    fmap snd (archiving `inject` deboostrapping)
