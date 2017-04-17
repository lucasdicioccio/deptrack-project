{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StaticPointers        #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where

import           Control.Applicative         (liftA2)
import           Control.Distributed.Closure
import           Control.Monad               (mapM)
import qualified Data.Binary                 as Bin
import           Data.Functor
import           Data.Monoid
import           Data.String.Conversions     (convertString)
import           Data.Text                   (pack, unpack)
import           Data.Typeable
import           DepTrack
import           Devops.Actions
import           Devops.Archive
import           Devops.Base
import           Devops.Binary
import           Devops.Bootstrap
import           Devops.Bootstrap.Build
import           Devops.Bootstrap.DO
import           Devops.Bootstrap.Parasite
import           Devops.Callback
import           Devops.Cli
import           Devops.Debian
import           Devops.Debian.User
import           Devops.Docker
import           Devops.Downloads
import           Devops.Haskell
import           Devops.Networking           hiding (Remoted)
import           Devops.Optimize             (optimizeDebianPackages)
import           Devops.OS
import           Devops.Ref
import           Devops.Service
import           Devops.Storage
import           Devops.Utils
import           GHC.Generics
import           Network.DO
import           System.Environment          (getArgs, getExecutablePath)
import           System.FilePath

exe    = "deptrack-devops-example-spark"
root   = preExistingUser "root"
allIps = "0.0.0.0"
dropletConfig key=  standardDroplet { size = G2, configImageSlug = ubuntuXenialSlug, keys = [key] }
sparkDistributionURL = "http://d3kbcqa49mib13.cloudfront.net/spark-1.6.1-bin-hadoop2.6.tgz"

deptrackProject :: DevOp (StackProject "deptrack-project")
deptrackProject = do
    let url = "https://github.com/begriffs/postgrest.git" 
    let branch = "master"
    stackProject url branch "deptrack-devops-example-spark" (mereUser "user")

instance HasBinary (StackProject "deptrack-project") "deptrack-devops-example-spark" where

selfBin :: DevOp (Binary "deptrack-devops-example-spark")
selfBin = stackInstall "/home/user" deptrackProject

dockerContent :: DevOp FilePresent
dockerContent = binaryPresent selfBin

parasitedHost :: FilePath -> String -> Int -> DevOp ParasitedHost
parasitedHost selfPath dropletName key = do
  let host     = droplet False ((dropletConfig key) { configName = dropletName } )
      build    = dockerized "sparkcluster-deptrack-build"
                            (selfCallback selfPath "~~~")
                            (preExistingDockerImage "haskell:8.0.2")
                            (closure $ static dockerContent)
      built    = fetchFile "/opt/self-bin" build
      doref    = saveRef (pack dropletName)
      resolved = resolveRef doref host
  parasite root built resolved

instance HasBinary (DebianPackage "openjdk-8-jdk-headless") "java" where

javaInstalled = let java = binary :: DevOp (Binary "java")
                    jre = debianPackage :: DevOp (DebianPackage "openjdk-8-jdk-headless")
                in java `installedWith` jre

-- TODO change to safer user
sparkUser  = root
sparkGroup = preExistingGroup "root"
installDir = homeDirPath . userName <$> sparkUser

sparkInstalled = let distrib    = do
                       base <- installDir
                       download sparkDistributionURL (base </> "/spark-1.6.1.tgz")
                     sparkDir   = do
                       base <- installDir
                       directory $ base </> "spark"
                 in  untar 1 (FilePresent . downloadedTo <$> distrib) sparkDir

ipv4Precedence = fileContent "/etc/gai.conf" (pure "precedence ::ffff:0:0/96 100\n")

data Spark

data SparkRole = SparkMaster | SparkSlave
  deriving (Generic, Typeable)

instance Bin.Binary SparkRole

data SparkConfig = SparkConfig { sparkRole     :: SparkRole
                               , sparkMasterIP :: IpNetString
                               , sparkOwnIP    :: IpNetString
                               , sparkArgs     :: [ String ]
                               } deriving (Generic, Typeable)

instance Bin.Binary SparkConfig

sparkCommandLine :: SparkConfig -> [ String ]
sparkCommandLine (SparkConfig SparkMaster _       ownIp args) = "-h" : unpack ownIp : args
sparkCommandLine (SparkConfig SparkSlave masterIp ownIp args) = url : "-h" : unpack ownIp : args
  where
    url = unpack $ "spark://" <> masterIp <> ":7077"

sparkStartCommand :: SparkConfig -> String
sparkStartCommand (SparkConfig SparkMaster _ _ _) = "start-master.sh"
sparkStartCommand (SparkConfig SparkSlave  _ _ _) = "start-slave.sh"

sparkStopCommand :: SparkConfig -> String
sparkStopCommand (SparkConfig SparkMaster _ _ _) = "stop-master.sh"
sparkStopCommand (SparkConfig SparkSlave  _ _ _) = "stop-slave.sh"

defaultSparkConfig = SparkConfig SparkMaster allIps allIps []

type instance DaemonConfig Spark = SparkConfig

instance Static (Serializable SparkConfig) where
  closureDict = closure $ static Dict

sparkNodeRunning :: SparkConfig -> DevOp (Binary "spark") -> DevOp (Daemon Spark)
sparkNodeRunning config sparkRootDir = devop id mkOp $ do
  _ <- ipv4Precedence  -- https://www.digitalocean.com/community/questions/how-to-disable-ubuntu-14-04-ipv6
  _ <- javaInstalled   -- we need java to be installed
  Binary path <- sparkRootDir
  return $ Daemon "spark" path Nothing config
    where
      mkOp (Daemon "spark" path Nothing config) = buildOp
        ("spark-master: " <> pack path)
        ("control spark process  " <> pack path <> " " <> pack (unwords $ sparkCommandLine config))
        noCheck   -- need to run start-master.sh and check output contains PID
        (startSpark path config)
        (stopSpark path config)
        noAction

      java_home_env = [("JAVA_HOME","/usr/lib/jvm/java-1.8.0-openjdk-amd64/")] -- TODO compute from installed java package

      startSpark path config = blindRunWithEnv (Binary $ path </> sparkStartCommand config) (sparkCommandLine config) "" java_home_env

      stopSpark path config  = blindRunWithEnv (Binary $ path </> sparkStopCommand config) [] "" java_home_env

sparkBin :: DevOp DirectoryPresent -> DevOp (Binary "spark")
sparkBin sparkDir = track mkOp $ do
  (DirectoryPresent d) <- sparkDir
  return $ bin $ d </> "sbin"
    where
      mkOp (Binary path) = noop ("spark-sbin: " <> pack path)
                           ("run spark nodes from " <> pack path)

-- | Install a spark master on "local" host
sparkMaster :: DevOp ParasitedHost -> DevOp (Remoted ())
sparkMaster host =
  let remoteMaster = closure (static (flip sparkNodeRunning (sparkBin sparkInstalled)))
      masterConfig = fst <$> track mkOp (do
                                           (ParasitedHost _ _ r') <- host
                                           return (do
                                                      myIp     <- remoteIp <$> resolver r'
                                                      return $ SparkConfig SparkMaster myIp myIp []
                                                  , r'))

      mkOp (_ , r') = noop ("master-config: @" <> pack (show $ resolvedKey r'))
                      ("configuration for spark master @" <> pack (show $ resolvedKey r'))

  in remotedWith masterConfig root remoteMaster host

sparkSlave :: DevOp (Remoted ()) -> DevOp ParasitedHost -> DevOp (Remoted ())
sparkSlave remoteMaster host =
  let remoteSlave = closure (static (flip sparkNodeRunning (sparkBin sparkInstalled)))
      slaveConfig = fst <$> track mkOp (do
                                           (Remoted r _)          <- remoteMaster
                                           (ParasitedHost _ _ r') <- host
                                           return (do
                                                      masterIP <- remoteIp <$> resolver r
                                                      myIp     <- remoteIp <$> resolver r'
                                                      return $ SparkConfig SparkSlave masterIP myIp []
                                                  , (r,r')))

      mkOp (_ , (r,r')) = noop ("slave-config: @" <> pack (show $ resolvedKey r'))
                          ("configuration for spark slave @" <> pack (show $ resolvedKey r') <> " with master " <> pack (show $ resolvedKey r))

  in fst <$> inject (remotedWith slaveConfig root remoteSlave host) remoteMaster

sparkCluster :: FilePath -> Int -> String -> Int -> DevOp ()
sparkCluster selfPath numberOfSlaves baseName key = do
  let masterHost      = parasitedHost selfPath (baseName <> "-master") key
      slaveHosts      = map (\ i -> parasitedHost selfPath (baseName <> "-slave-" <> show i) key) [1 .. numberOfSlaves]
      sparkMasterHost = sparkMaster masterHost
  void $ traverse (sparkSlave sparkMasterHost) slaveHosts

main :: IO ()
main = do
  args <- getArgs
  currentSelf <- getExecutablePath
  go currentSelf args
  where
    go currentSelf xs
        | isMagicRemoteArgv xs = base64EncodedRemoteSetup (drop 1 xs)
        | otherwise            = localSetup currentSelf xs

    base64EncodedRemoteSetup :: [String] -> IO ()
    base64EncodedRemoteSetup (b64:[]) = void $ do
        let target = opFromClosureB64 (convertString b64)
        defaultMain target [optimizeDebianPackages] ["up"]
    base64EncodedNestedSetup _ = Prelude.error "invalid args for magic droplet callback"

    localSetup :: FilePath -> [String] -> IO ()
    localSetup currentSelf (numSlaves:baseName:key:args) =
      defaultMain (sparkCluster currentSelf (read numSlaves) baseName (read key))  [optimizeDebianPackages] args
