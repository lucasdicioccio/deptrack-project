{-# LANGUAGE RecordWildCards #-}
module Devops.Utils.Docker where

import           Data.Char   (toLower)
import           Data.List   (foldl1', intercalate)
import           Data.Monoid ((<>))

-- | Name of interface or IP address, maybe 0.0.0.0 to bind to all available interfaces
type IPInterface = String

allInterfaces :: IPInterface
allInterfaces = "0.0.0.0"

localhost :: IPInterface
localhost = "127.0.0.1"

type Port = Int

data IPProto = UDP | TCP deriving (Show)

data Endpoint = Endpoint { _ip   :: IPInterface
                         , _port :: Port
                         }
              deriving (Eq, Show, Read)

hostport :: Endpoint -> String
hostport Endpoint{..} = _ip <> ":" <> show _port

-- | A Docker image reference
newtype ImageName     = ImageName { imageName :: String } deriving (Eq)

instance Show ImageName where
  show = imageName

instance Read ImageName where
  readsPrec _ s = [(ImageName s,"")]

-- | A Docker container name
newtype ContainerName = ContainerName { containerName :: String } deriving (Eq)

instance Show ContainerName where
  show = containerName

instance Read ContainerName where
  readsPrec _ s = [(ContainerName s, "")]


dockerParams :: RunParam -> [String]
dockerParams NoParam    = []
dockerParams Detach     = ["-d"]
dockerParams (Restart policy)
                        = ["--restart=" <> show policy]
dockerParams Port{..}   = ["-p", (intercalate ":" [hostInterface, show hostPort, show containerPort] ++ "/" ++ (map toLower $ show proto))]
dockerParams Volume{..} = ["-v", hostPath <> ":" <> containerPath]
dockerParams Link{..}   = ["--link=" ++ show linkedName <> ":" <> show linkeeName]
dockerParams Name{..}   = ["--name=" ++ show contName]
dockerParams Image{..}  = [show dockerImage]
dockerParams LogConfig{..} = ("--log-driver=" ++ show logDriver) : concatMap paramsLogOptions logOptions
  where
    paramsLogOptions (k,v) = ["--log-opt",k ++ "=" ++ v]

dockerParams (p :-- p') = dockerParams p ++ dockerParams p'

infixl 2 :--

data RunParam = Port { hostInterface :: IPInterface, hostPort :: Port, containerPort :: Port, proto :: IPProto }
              | Volume { hostPath :: FilePath, containerPath :: FilePath }
              | Link { linkedName :: ContainerName, linkeeName :: ContainerName }
              | Name { contName :: ContainerName }
              | Image { dockerImage :: ImageName }
              | Restart { restartPolicy :: RestartPolicy }
              | LogConfig { logDriver :: LogDriver, logOptions :: [ LogOption ] }
              | RunParam :-- RunParam
              | Detach
              | NoParam

instance Monoid RunParam where
  mempty = NoParam
  mappend = (:--)

-- | Available drivers
--
-- see http://docs.docker.com/engine/reference/logging/overview/#the-json-file-options
data LogDriver = JsonFile
               | Syslog
               | Journald
               | Gelf
               | Fluentd
               | Awslogs

instance Show LogDriver where
  show JsonFile = "json-file"
  show Syslog   = "syslog"
  show Journald = "journald"
  show Gelf     = "gelf"
  show Fluentd  = "fluentd"
  show Awslogs  = "awslogs"

type LogOption = (String, String)

data RestartPolicy = NeverRestart
                   | OnFailure { maxRetries :: Int }
                   | AlwaysRestart

instance Show RestartPolicy where
  show NeverRestart   = "no"
  show (OnFailure mr) = "on-failure:"++ show mr
  show AlwaysRestart  = "always"

ip :: IPInterface -> RunParam -> RunParam
ip iface p@Port{..} = p { hostInterface = iface }
ip _     p          = p

port :: Port -> RunParam
port p = Port allInterfaces p p TCP

udp :: RunParam -> RunParam
udp p@Port{} = p { proto = UDP }
udp r        = r

volume :: FilePath -> FilePath -> RunParam
volume = Volume

link :: ContainerName -> ContainerName -> RunParam
link = Link

name :: String -> RunParam
name = Name . ContainerName

detach :: RunParam
detach = Detach

restart :: RestartPolicy -> RunParam
restart = Restart

logConfig :: LogDriver -> [ LogOption ] -> RunParam
logConfig = LogConfig

container :: [RunParam] -> RunParam
container [] =  NoParam
container ps = foldl1' (:--) ps
