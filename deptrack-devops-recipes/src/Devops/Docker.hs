{-# LANGUAGE OverloadedStrings #-}

module Devops.Docker (
    DockerImage
  , dockerImage
  ) where

import           Data.Monoid ((<>))
import           Data.String.Conversions (convertString)

import           Devops.Base
import           Devops.BaseImage
import qualified Devops.Debian.Commands as Cmd
import           Devops.DockerBootstrap
import           Devops.Utils
import           Devops.Storage

data DockerImage = DockerImage Name

-- | A named Docker image from a simple base.
--
-- TODO: consider relaxing BaseImage's FilePresent to a URL (e.g., using a
-- TypeFamily to specialize the type). Indeed, we can download a BaseImage from
-- a repository using Docker.
dockerImage :: Name -> DevOp (BaseImage DockerBase) -> DevOp (DockerImage)
dockerImage name mkBase = devop fst mkOp $ do
    base <- mkBase
    docker <- Cmd.docker
    return (DockerImage name, (docker, base))
  where
    mkOp (_, (docker, base)) =
        let path = getFilePresentPath (imagePath base) in
        let hasName n dat = n `elem` lines dat in
        buildOp ("docker-image: " <> name)
                ("imports " <> convertString path <> " as " <> name)
                (checkBinaryExitCodeAndStdout (hasName $ convertString name)
                     docker [ "images"
                            , "--format", "{{title .Repository}}"
                            ]
                            "")
                (blindRun docker ["import", path, convertString name] "")
                (blindRun docker ["rmi", convertString name] "")
                noAction
