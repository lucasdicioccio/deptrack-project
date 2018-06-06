{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}

module Devops.Docker (
    DockerImage (..)
  , dockerImage
  , preExistingDockerImage
  , pulledDockerImage
  , Container (..)
  , ContainerCommand (..)
  , StandbyContainer (..)
  , standbyContainer
  , RunningContainer (..)
  , runningContainer
  , DockerWaitMode (..)
  , Dockerized (..)
  , dockerized
  , insertFile
  , insertDir
  , dockerizedDaemon
  , committedImage
  , fetchFile
  , fetchLogs
  , resolveDockerRemote
  , DockerMachine (..)
  , dockerMachine
  , resolveDockerEnv
  ) where

import           Control.Monad (guard)
import           Control.Applicative ((<|>))
import           Data.Aeson
import qualified Data.List               as List
import           Data.Maybe (catMaybes, isJust)
import           Data.Monoid ((<>))
import           Data.String.Conversions (convertString)
import qualified Data.Text               as Text
import           System.FilePath.Posix   ((</>))
import           System.Process          (readProcess)

import           Devops.Base
import           Devops.BaseImage
import           Devops.Binary
import           Devops.Callback
import           Devops.Cli
import           Devops.Constraints (HasOS, onOS)
import qualified Devops.Debian.Commands as Debian
import qualified Devops.MacOS.Commands as MacOS
import           Devops.DockerBootstrap
import           Devops.Networking
import           Devops.Utils
import           Devops.Ref
import           Devops.Service
import           Devops.Storage

data DockerMachine = DockerMachine !Name

dockerMachine :: HasOS env => Name -> DevOp env (DockerMachine, Binary "docker-machine")
dockerMachine name = devop id mkOp $ do
    dockm <- onOS "mac-os" $ MacOS.dockerMachine
    _     <- onOS "mac-os" $ MacOS.vboxManage
    return (DockerMachine name, dockm)
  where
    mkOp (_, dockm) =
        buildOp ("docker-machine: " <> name)
                ("creates docker machine " <> name)
                verifyExists
                (create >> start)
                delete
                noAction
        where
            create = blindRun dockm ["create", "-d", "virtualbox", "--virtualbox-memory", "2048", convertString name] ""
            start  = blindRun dockm ["start", convertString name] ""
            delete = blindRun dockm ["rm", convertString name] ""
            verifyExists =
                let listsName dat = convertString name `elem` lines dat
                in (checkBinaryExitCodeAndStdout listsName
                     dockm [ "ls" , "--format", "{{.Name}}" ] "")

type DockerMachineEnv = [(String, String)]
instance HasResolver DockerMachineEnv (DockerMachine, Binary dockm) where
    resolve _ (_,Binary dockm) = do
        dat <- readProcess dockm [ "env" ] ""
        seq (length dat) (return $ parseEnv dat)

parseEnv :: String -> [(String, String)]
parseEnv = catMaybes . fmap parseLine . lines
parseLine :: String -> Maybe (String, String)
parseLine orig = do
  rest <- List.stripPrefix "export " orig
  parsePair (List.break (=='=') rest)

parsePair :: (String, String) -> Maybe (String, String)
parsePair ("", xs)       = Nothing
parsePair (key, '=':val) = Just (key, unescape val)
parsePair _              = Nothing

unescape = id

resolveDockerEnv :: HasOS env => Name -> DevOp env (Resolver DockerMachineEnv)
resolveDockerEnv name =
    resolveRef (saveRef $ "docker-machine-ref:" <> name) (dockerMachine name)

-- | An OS-specific Docker binary.
docker :: HasOS env => DevOp env (Binary "docker")
docker = onOS "debian" Debian.docker <|> onOS "mac-os" (dockerMachine "default" >> MacOS.docker)

data DockerImage = DockerImage !Name

-- | A named, pre-existing docker image.
preExistingDockerImage :: Name -> DevOp env DockerImage
preExistingDockerImage name =
    declare op (return $ DockerImage name)
  where
    op = buildPreOp ("docker-image: " <> name)
                    ("pre-existing Docker image" <> name)
                    noCheck
                    noAction
                    noAction
                    noAction

-- | An image pulled from the Internet.
pulledDockerImage :: HasOS env => Name -> DevOp env DockerImage
pulledDockerImage name = devop fst mkOp $ do
    dock <- docker
    return (DockerImage name, dock)
  where
    mkOp (_, dock) =
        let hasName n dat = n `elem` lines dat in
        buildOp ("docker-pulled-image: " <> name)
                ("pulls " <> name <> " from the docker hub ")
                (checkBinaryExitCodeAndStdout (hasName $ convertString name)
                     dock [ "images"
                          , "--format", "{{title .Repository}}"
                          ]
                          "")
                (blindRun dock ["pull", convertString name] "")
                (blindRun dock ["rmi", convertString name] "")
                noAction

-- | A named Docker image from a simple base.
--
-- TODO: consider relaxing BaseImage's FilePresent to a URL (e.g., using a
-- TypeFamily to specialize the type). Indeed, we can download a BaseImage from
-- a repository using Docker.
dockerImage :: HasOS env => Name -> DevOp env (BaseImage DockerBase) -> DevOp env (DockerImage)
dockerImage name mkBase = devop fst mkOp $ do
    base <- mkBase
    dock <- docker
    return (DockerImage name, (dock, base))
  where
    mkOp (_, (dock, base)) =
        let path = getFilePresentPath (imagePath base) in
        let hasName n dat = n `elem` lines dat in
        buildOp ("docker-image: " <> name)
                ("imports " <> convertString path <> " as " <> name)
                (checkBinaryExitCodeAndStdout (hasName $ convertString name)
                     dock [ "images"
                          , "--format", "{{title .Repository}}"
                          ]
                          "")
                (blindRun dock ["import", path, convertString name] "")
                (blindRun dock ["rmi", convertString name] "")
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

newtype StandbyContainer = StandbyContainer { getStandby :: Container }
newtype RunningContainer = RunningContainer { getRunning :: Container }

-- | Spawns a container for running a command but does not start it.
--
-- It's cid is stored in a file in /var/run/devops.
standbyContainer :: HasOS env => Name
                 -> DevOp env DockerImage
                 -> DevOp env ContainerCommand
                 -> DevOp env StandbyContainer
standbyContainer name mkImage mkCmd = devop fst mkOp $ do
    DirectoryPresent dirPath <- directory "/var/run/devops"
    let cidFile = dirPath </> Text.unpack name <> ".le-cid"
    image@(DockerImage imageName) <- mkImage
    cmd <- mkCmd
    dock <- docker
    return $ (StandbyContainer $ Container name cidFile image cmd, (dock, imageName))
  where
    mkOp (StandbyContainer (Container _ cidFile _ cmd), (dock, imageName)) =
        let (potentialCopy, callbackPath, args) = case cmd of
                (ExistingContainerCommand fp argv) -> (return (), fp, argv)
                (ImportedContainerCommand (FilePresent srcBin) argv) ->
                    let cb = "/devops-callback"
                        action = do
                            blindRun dock [ "cp" , srcBin
                                          , convertString name <> ":" <> cb ] ""
                    in  (action, cb, argv)
            volumes = [] -- TODO: pass -v  somePath<>":/mount" optionally
        in
        buildOp ("docker-container-standby: " <> name)
                ("creates " <> name <> " from image " <> imageName <> " with args: " <> convertString (show callbackPath) <> " " <> convertString (show args))
                (checkFilePresent cidFile)
                (blindRun dock ("create" : volumes ++ [
                                  "--cidfile" , cidFile
                                , "--name" , convertString name
                                , convertString imageName
                                , callbackPath
                                ] ++ args ) ""
                >> potentialCopy)
                (blindRemoveLink cidFile
                 >> blindRun dock [ "rm" , convertString name ] "")
                noAction

-- | Starts a standing-by container and wait for it depending on a waitmode.
runningContainer :: HasOS env => DockerWaitMode
                 -> DevOp env StandbyContainer
                 -> DevOp env RunningContainer
runningContainer waitmode standby = devop fst mkOp $ do
    running <- RunningContainer . getStandby <$> standby
    dock <- docker
    return $ (running, dock)
  where
    mkOp (RunningContainer (Container name cidFile _ _), dock) =
        let waitAction = case waitmode of
                NoWait -> return ()
                Wait -> blindRun dock [ "wait" , convertString name ] ""
        in
        buildOp ("docker-container-running: " <> name)
                ("starts " <> name)
                (checkFilePresent cidFile)
                (blindRun dock [ "start" , convertString name ] ""
                 >> waitAction)
                (blindRemoveLink cidFile
                 >> blindRun dock [ "stop" , convertString name ] "")
                (blindRun dock [ "restart" , convertString name ] ""
                 >> waitAction)

data Dockerized a =
    Dockerized { dockerizedObj       :: !a
               , dockerizedContainer :: !Container
               } deriving Functor

insertFile :: HasOS env => DevOp env FilePresent
           -> FilePath
           -> DevOp env StandbyContainer
           -> DevOp env StandbyContainer
insertFile mkFile tgt mkStandby = devop fst mkOp $ do
    (FilePresent local) <- mkFile
    standby <- mkStandby
    dock <- docker
    return (standby, (dock, convertString local))
  where
    mkOp (standby, (dock, local)) =
        let name = containerName . getStandby $ standby in
        buildOp ("upload-file: " <> local)
                ("upload " <> local <> " in container " <> name)
                noCheck
                (blindRun dock [ "cp", convertString local , convertString name <> ":" <> tgt ] "")
                noAction
                noAction

insertDir :: HasOS env => DevOp env DirectoryPresent
          -> FilePath
          -> DevOp env StandbyContainer
          -> DevOp env StandbyContainer
insertDir mkDir tgt mkStandby = devop fst mkOp $ do
    (DirectoryPresent local) <- mkDir
    standby <- mkStandby
    dock <- docker
    return (standby, (dock, convertString local))
  where
    mkOp (standby, (dock, local)) =
        let name = containerName . getStandby $ standby in
        buildOp ("upload-dir: " <> local)
                ("upload " <> local <> " in container " <> name)
                noCheck
                (blindRun dock [ "cp", convertString local , convertString name <> ":" <> tgt ] "")
                noAction
                noAction


dockerized :: HasOS env => Name
           -- ^ the name of the docker container
           -> DevOp env DockerImage
           -- ^ the image used to start this container
           -> Continued env a
           -- ^ the continued program to run in the container
           -> env
           -> (DevOp env StandbyContainer -> DevOp env StandbyContainer)
           -- ^ an optional setup phase to modify the container. Use 'id' for
           -- "no particular setup" or partially-apply 'insertFile' to add
           -- extra data.
           -> DevOp env (Dockerized (Maybe a))
dockerized name mkImage cont env beforeStart = declare op $ do
    let obj = eval env cont
    let (BinaryCall selfPath fArgs) = callback cont
    let args = fArgs (TurnUp Concurrently)
    let selfBin = preExistingFile selfPath
    let mkCmd = ImportedContainerCommand <$> selfBin <*> pure args
    let standby = standbyContainer name mkImage mkCmd
    let container = getRunning  <$> runningContainer Wait (beforeStart standby)
    Dockerized obj <$> container
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
    resolve _ (DockerizedDaemon _ _ _,Binary dock) = do
        dat <- readProcess dock [ "network", "inspect", "bridge" ] ""
        seq (length dat) (return $ convertString dat)

resolveDockerRef :: HasOS env => DevOp env (DockerizedDaemon a)
                 -> DevOp env (Resolver (DockerBridgeInfo a))
resolveDockerRef service =
    resolveRef ref ((,) <$> service <*> docker)
  where
    ref = fmap _daemonBridgeInfo service

resolveDockerRemote :: HasOS env => DevOp env (DockerizedDaemon a) -> DevOp env (Resolver (Remoted a))
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
--
-- The Continued below will get called with 'Upkeep' rather than 'TurnUp' so
-- that the main forked thread does not die.
dockerizedDaemon
  :: HasOS env => Name
  -> DevOp env DockerImage
  -> Continued env (f (Daemon a))
  -> env
  -> (DevOp env StandbyContainer -> DevOp env StandbyContainer)
  -> DevOp env (DockerizedDaemon (Maybe (f (Daemon a))))
dockerizedDaemon name mkImage cont env beforeStart = declare op $ do
    let obj = eval env cont
    let (BinaryCall selfPath fArgs) = callback cont
    let args = fArgs Upkeep
    let selfBin = preExistingFile selfPath
    let mkCmd = ImportedContainerCommand <$> selfBin <*> pure args
    let ref = saveRef ("dockerized-daemon-ref: " <> name)
    let standby = standbyContainer name mkImage mkCmd
    let container = getRunning <$> runningContainer NoWait (beforeStart standby)
    DockerizedDaemon obj <$> container <*> ref
  where
    op = buildPreOp ("dockerized-daemon: " <> name)
                    ("dockerize and let run some node in the Docker image:" <> name)
                    noCheck
                    noAction
                    noAction
                    noAction

-- | Commit an image from a container.
committedImage :: HasOS env => DevOp env (Dockerized a) -> DevOp env DockerImage
committedImage mkDockerized = devop fst mkOp $ do
    (Dockerized _ cntner) <- mkDockerized
    let (DockerImage baseImageName) = containerImage cntner
    let name = baseImageName <> "--" <> containerName cntner
    dock <- docker
    return $ (DockerImage name, (dock, cntner))
  where
    mkOp (DockerImage name, (dock, cntner)) =
        let hasName n dat = n `elem` lines dat in
        buildOp ("docker-image: " <> name)
                ("creates " <> name <> " based on container " <> containerName cntner)
                (checkBinaryExitCodeAndStdout (hasName $ convertString name)
                     dock [ "images"
                            , "--format", "{{title .Repository}}"
                            ]
                            "")
                (blindRun dock ["commit", convertString $ containerName cntner, convertString name] "")
                (blindRun dock ["rmi", convertString name] "")
                noAction

-- | Fetches a file from a container.
fetchFile :: HasOS env => FilePath -> DevOp env (Dockerized (Maybe FilePresent)) -> DevOp env (FilePresent)
fetchFile path mkFp =
    fmap f (generatedFile path docker mkArgs)
  where
    f (_,_,filepresent) = filepresent
    mkArgs = do
        (Dockerized cPath cntnr) <- mkFp
        guard $ isJust cPath
        let Just (FilePresent containerizedPath) = cPath
        return [ "container"
               , "cp"
               , (convertString $ containerName cntnr) <> ":" <> containerizedPath
               , path
               ]

-- | Fetches the logs from a container.
fetchLogs :: HasOS env => FilePath -> DevOp env (Dockerized a) -> DevOp env FilePresent
fetchLogs path mkDock = ioFile path io
  where
    io = do
        dock <- docker
        (Dockerized _ cntnr) <- mkDock
        return $ convertString <$> readProcess (binaryPath dock) ["logs", convertString $ containerName cntnr] ""
