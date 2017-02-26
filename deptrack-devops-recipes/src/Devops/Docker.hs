{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Devops.Docker (
    DockerImage
  , dockerImage
  , preExistingDockerImage
  , Container (..)
  , ContainerCommand (..)
  , container
  ) where

import           Data.Monoid ((<>))
import           Data.String.Conversions (convertString)
import qualified Data.Text               as Text
import           DepTrack (declare)
import           System.FilePath.Posix   ((</>))

import           Devops.Base
import           Devops.BaseImage
import qualified Devops.Debian.Commands as Cmd
import           Devops.DockerBootstrap
import           Devops.Utils
import           Devops.Storage

data DockerImage = DockerImage !Name

-- | A named, pre-existing docker image.
preExistingDockerImage :: Name -> DevOp DockerImage
preExistingDockerImage name =
    declare op (return $ DockerImage name)
  where
    op = buildPreOp ("docker-image: " <> name)
                    ("pre-existing Docker image" <> name)
                    noCheck
                    noAction
                    noAction
                    noAction

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

-- | Docker containers are built from an image and
data Container =
    Container { containerName    :: !Name
              , containerCidPath :: !FilePath
              , containerImage   :: !DockerImage
              , containerCommand :: !ContainerCommand
              }

data ContainerCommand =
    ImportedContainerCommand !FilePresent [String]
  | ExistingContainerCommand !FilePath [String]

-- | Spawns a container for running a command.
-- The command is immediately started and the container is disposed only at
-- turndown.
--
-- It's cid is stored in a file in /var/run/devops.
container :: Name
          -> DevOp DockerImage
          -> DevOp ContainerCommand
          -> DevOp Container
container name mkImage mkCmd = devop fst mkOp $ do
    DirectoryPresent dirPath <- directory "/var/run/devops"
    let cidFile = dirPath </> Text.unpack name <> ".le-cid"
    image@(DockerImage imageName) <- mkImage
    cmd <- mkCmd
    docker <- Cmd.docker
    return $ (Container name cidFile image cmd, (docker, imageName))
  where
    mkOp (Container _ cidFile _ cmd, (docker, imageName)) =
        let (potentialCopy, callbackPath, args) = case cmd of
                (ExistingContainerCommand fp argv) -> (return (), fp, argv)
                (ImportedContainerCommand (FilePresent srcBin) argv) ->
                    let cb = "/devops-callback"
                        action = blindRun docker [ "cp" , srcBin
                                                 , convertString name <> ":" <> cb ] ""
                    in  (action, cb, argv)
        in
        buildOp ("docker-container: " <> name)
                ("creates " <> name <> " from image " <> imageName)
                (checkFilePresent cidFile)
                (blindRun docker ([ "create"
                                 , "--cidfile" , cidFile
                                 , "--name" , convertString name
                                 , convertString imageName
                                 , callbackPath
                                 ] ++ args )""
                 >> potentialCopy
                 >> blindRun docker [ "start" , convertString name ] "")
                (blindRemoveLink cidFile
                 >> blindRun docker [ "rm" , convertString name ] "")
                (blindRun docker [ "restart" , convertString name ] "")
