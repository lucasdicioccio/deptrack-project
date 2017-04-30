{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}

module Devops.Docker (
    DockerImage
  , dockerImage
  , preExistingDockerImage
  , Container (..)
  , ContainerCommand (..)
  , container
  , DockerWaitMode (..)
  , Dockerized (..)
  , dockerized
  , dockerizedDaemon
  , committedImage
  , fetchFile
  , resolveDockerRemote
  ) where

import           Data.Aeson
import           Data.Monoid ((<>))
import           Data.String.Conversions (convertString)
import qualified Data.Text               as Text
import           DepTrack (declare)
import           System.FilePath.Posix   ((</>))
import           System.Process          (readProcess)

import           Devops.Base
import           Devops.BaseImage
import           Devops.Binary
import           Devops.Callback
import qualified Devops.Debian.Commands as Cmd
import           Devops.DockerBootstrap
import           Devops.Networking
import           Devops.Utils
import           Devops.Ref
import           Devops.Service
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

-- | Docker containers are built from an image and run a given command.
data Container =
    Container { containerName    :: !Name
              , containerCidPath :: !FilePath
              , containerImage   :: !DockerImage
              , containerCommand :: !ContainerCommand
              }

data ContainerCommand =
    ImportedContainerCommand !FilePresent [String]
  | ExistingContainerCommand !FilePath [String]

data DockerWaitMode =
    NoWait
  | Wait

-- | Spawns a container for running a command.
-- The command is immediately started and the container is disposed only at
-- turndown.
--
-- It's cid is stored in a file in /var/run/devops.
container :: Name
          -> DockerWaitMode
          -> DevOp DockerImage
          -> DevOp ContainerCommand
          -> DevOp Container
container name waitmode mkImage mkCmd = devop fst mkOp $ do
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
                        action = do
                            blindRun docker [ "cp" , srcBin
                                            , convertString name <> ":" <> cb ] ""
                    in  (action, cb, argv)
            waitAction = case waitmode of
                NoWait -> return ()
                Wait -> blindRun docker [ "wait" , convertString name ] ""
        in
        buildOp ("docker-container: " <> name)
                ("creates " <> name <> " from image " <> imageName <> " with args: " <> convertString (show callbackPath) <> " " <> convertString (show args))
                (checkFilePresent cidFile)
                (blindRun docker ([ "create"
                                 , "--cidfile" , cidFile
                                 , "--name" , convertString name
                                 , convertString imageName
                                 , callbackPath
                                 ] ++ args )""
                 >> potentialCopy
                 >> blindRun docker [ "start" , convertString name ] ""
                 >> waitAction)
                (blindRemoveLink cidFile
                 >> blindRun docker [ "rm" , convertString name ] "")
                noAction

data Dockerized a = Dockerized !a !Container

dockerized :: Name
           -> DevOp DockerImage
           -> Continued a
           -> DevOp (Dockerized a)
dockerized name mkImage cont = declare op $ do
    let obj = eval cont
    (BinaryCall selfPath args) <- callback cont
    let selfBin = preExistingFile selfPath
    let mkCmd = ImportedContainerCommand <$> selfBin <*> pure args
    let cbContainer = container name Wait mkImage mkCmd
    Dockerized obj <$> cbContainer
  where
    op = buildPreOp ("dockerized-node: " <> name)
                    ("dockerize some node in the Docker image:" <> name)
                    noCheck
                    noAction
                    noAction
                    noAction

type DockerBridgeInfo b = String
data DockerizedDaemon a =
    DockerizedDaemon { _daemonVal       :: !a
                     , _daemonContainer :: !Container
                     , _daemonBridgeInfo:: !(Ref (DockerBridgeInfo a))
                     }

instance HasResolver (DockerBridgeInfo b) (DockerizedDaemon b, Binary docker) where
    resolve _ (DockerizedDaemon _ _ _,Binary docker) = do
        dat <- readProcess docker [ "network", "inspect", "bridge" ] ""
        seq (length dat) (return $ convertString dat)

resolveDockerRef :: DevOp (DockerizedDaemon a)
                 -> DevOp (Resolver (DockerBridgeInfo a))
resolveDockerRef service =
    resolveRef ref ((,) <$> service <*> Cmd.docker)
  where
    ref = fmap _daemonBridgeInfo service

resolveDockerRemote :: DevOp (DockerizedDaemon a) -> DevOp (Resolver (Remoted a))
resolveDockerRemote mkService = do
    (DockerizedDaemon service _ _) <- mkService
    (fmap . fmap) (g service) (resolveDockerRef mkService)
  where
    g val bridgeInfo = Remoted (Remote (parseDockerIp bridgeInfo)) val
    parseDockerIp :: DockerBridgeInfo a -> IpNetString
    parseDockerIp = unsafeLookup . decode . convertString
    unsafeLookup :: Maybe Value -> IpNetString
    unsafeLookup (Just (Array _)) = "172.17.0.2" -- TODO: actually parse
    unsafeLookup _ = error "could not decode"

-- | Sets up a Daemon in a given Docker container.
--
-- This implementation does not wait for the container callback to terminate
-- during turnup (hence, a Daemon).
--
-- The functorial wrapper around the Daemon allows to setup networked daemons
-- such as Listening (Daemon) from the Devops.Networking package.
dockerizedDaemon :: Name
                 -> DevOp DockerImage
                 -> Continued (f (Daemon a))
                 -> DevOp (DockerizedDaemon (f (Daemon a)))
dockerizedDaemon name mkImage cont = declare op $ do
    let obj = eval cont
    (BinaryCall selfPath args) <- callback cont
    let selfBin = preExistingFile selfPath
    let mkCmd = ImportedContainerCommand <$> selfBin <*> pure args
    let cbContainer = container name NoWait mkImage mkCmd
    let ref = saveRef ("dockerized-daemon-ref: " <> name)
    DockerizedDaemon obj <$> cbContainer <*> ref
  where
    op = buildPreOp ("dockerized-daemon: " <> name)
                    ("dockerize and let run some node in the Docker image:" <> name)
                    noCheck
                    noAction
                    noAction
                    noAction

-- | Commit an image from a container.
committedImage :: DevOp (Dockerized a) -> DevOp DockerImage
committedImage mkDockerized = devop fst mkOp $ do
    (Dockerized _ cntner) <- mkDockerized
    let (DockerImage baseImageName) = containerImage cntner
    let name = baseImageName <> "--" <> containerName cntner
    docker <- Cmd.docker
    return $ (DockerImage name, (docker, cntner))
  where
    mkOp (DockerImage name, (docker, cntner)) =
        let hasName n dat = n `elem` lines dat in
        buildOp ("docker-image: " <> name)
                ("creates " <> name <> " based on container " <> containerName cntner)
                (checkBinaryExitCodeAndStdout (hasName $ convertString name)
                     docker [ "images"
                            , "--format", "{{title .Repository}}"
                            ]
                            "")
                (blindRun docker ["commit", convertString $ containerName cntner, convertString name] "")
                (blindRun docker ["rmi", convertString name] "")
                noAction

-- | Fetches a file from a container.
fetchFile :: FilePath -> DevOp (Dockerized FilePresent) -> DevOp FilePresent
fetchFile path mkFp = fmap f (generatedFile path Cmd.docker mkArgs)
  where
    f (_,_,filepresent) = filepresent
    mkArgs = do
        (Dockerized (FilePresent containerizedPath) cntnr) <- mkFp
        return [ "container"
               , "cp"
               , (convertString $ containerName cntnr) <> ":" <> containerizedPath
               , path
               ]
