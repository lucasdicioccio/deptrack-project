{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Configs where

import           Control.Monad (void)
import           Data.Monoid ((<>))
import           Data.String.Conversions (convertString)
import           Devops.Base (DevOp, devop, buildOp, noAction, noCheck, Name, inject)
import           Devops.Callback (BinaryCall, binaryCall, continueConst)
import           Devops.Cli (App (..), Method (..), SelfPath, appMain, appMethod, methodArg)
import           Devops.Constraints (HasOS(..), onOS)
import qualified Devops.Debian.Commands as Debian
import           Devops.Debian.Base (deb)
import           Devops.Debian.User (mereUser, userDirectory)
import           Devops.Docker (DockerImage(..), pulledDockerImage, resolveDockerEnv)
import           Devops.Git (GitRepo(..), gitCloneSimple, gitClone, GitUrl)
import           Devops.Networking (IpNetString, existingRemote)
import           Devops.Optimize (optimizeDebianPackages)
import           Devops.Parasite (ControlledHost, control, parasite, remoted)
import           Devops.Ref
import           Devops.Storage (DirectoryPresent(..), directory)
import qualified Devops.StaticSite as StaticSite
import           Devops.MacOS.Base (brew)
import           Devops.Nginx (NginxServerConfig, HostName, reverseProxy)
import qualified Devops.MacOS.Commands as MacOS
import           Devops.Utils (blindRunWithEnv)
import           System.IO  (BufferMode (..), hSetBuffering, stderr, stdout)
import           System.Directory (makeAbsolute)
import           System.Process (readProcess)

-- | We configure MacbookAir an some Debian-like servers.
data Env = MacbookAir | DebianLike
instance HasOS Env where
    os MacbookAir = "mac-os"
    os DebianLike = "debian"

-- | We configure a laptop or a website-host server.
data Configuration = Laptop | Websites | DevMachine

discoverEnv :: IO Env
discoverEnv = do
    fmap (look . lines) $! readProcess "uname" [] ""
  where
    look ("Darwin":[]) = MacbookAir
    look ("Linux":[])  = DebianLike
    look _             = MacbookAir

parseStage :: [String] -> (Configuration, Method)
parseStage = \case
    (arg:[]) ->
        (Laptop, appMethod arg)
    ("websites":arg:[]) ->
        (Websites, appMethod arg)
    ("devmachine":arg:[]) ->
        (DevMachine, appMethod arg)
    args                ->
        error $ "unparsed args: " ++ show args

unparseStage :: Configuration -> Method -> [String]
unparseStage stage m = case stage of
    Laptop ->
        [ methodArg m ]
    Websites ->
        [ "websites", methodArg m ]
    DevMachine ->
        [ "devmachine", methodArg m ]

runMain :: IO ()
runMain = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    let app = App parseStage unparseStage stages [optimizeDebianPackages] (const $ discoverEnv)
    appMain app

stages
  :: Configuration
  -> SelfPath
  -> (Configuration -> Method -> [String])
  -> DevOp Env ()
stages Laptop self fixCall = do
    macbookAir
    let xremote u h x1 x2 = macbookCrossBuild (binaryCall self . fixCall) (remoteHost h u) x1 x2
    xremote "devop" "51.15.169.149" websites Websites
    xremote "devop" "51.15.165.203" websites Websites
    xremote "dicioccio" "62.210.82.229" devMachine DevMachine
stages Websites _ _  =
    websites
stages DevMachine _ _  =
    devMachine

-- | Setup enough for developing on my Macbook air.
macbookAir :: DevOp Env ()
macbookAir = onOS "mac-os" $ void $ do
    brew "tree"
    brew "tmux"
    MacOS.dot

devMachine :: DevOp Env ()
devMachine = onOS "debian" $ void $ do
    deb "tmux"
    deb "weechat"
    Debian.git

macbookCrossBuild
  :: (Configuration -> BinaryCall)
  -> DevOp Env ControlledHost
  -> DevOp Env ()
  -> Configuration
  -> DevOp Env ()
macbookCrossBuild cb host mkConf conf = onOS "mac-os" $ void $ do
    let fpcoImg = pulledDockerImage "fpco/stack-build"
    let cloneUrl = "https://github.com/lucasdicioccio/deptrack-project.git" 
    let mappedDir = gitDir <$> gitCloneSimple cloneUrl "master" MacOS.git (directory "cloning-src")
    let crosscompiledself = crossCompile "x-compile" fpcoImg mappedDir
    let parasitedServer = fmap fst $ parasite binPath host `inject` crosscompiledself
    remoted (continueConst mkConf $ cb conf) DebianLike parasitedServer `inject` crosscompiledself

-- | Cross compiles itself inside docker for Docker commands to run next.
crossCompile :: Name -> DevOp Env DockerImage -> DevOp Env DirectoryPresent -> DevOp Env ()
crossCompile name mkImg mkSrc = devop fst mkOp $ do
    docker <- MacOS.docker
    dockerMachineEnv <- resolveDockerEnv "default"
    img <- mkImg
    src <- mkSrc
    return ((), (docker,dockerMachineEnv, img,src))
  where
    mkOp (_,(docker, dockerEnvR, (DockerImage imgName), (DirectoryPresent srcPath))) = 
        buildOp ("cross-compile:" <> convertString srcPath)
                ("stack-builds " <> convertString srcPath <> " inside " <> imgName <> " as " <> name)
                (noCheck)
                runInDocker
                noAction
                noAction
        where
            runInDocker = do
                env <- resolver dockerEnvR
                print env
                path <- makeAbsolute srcPath
                let mountSpec = path <> ":" <> "/mount"
                blindRunWithEnv docker [
                    "run"
                  , "--name", convertString name
                  , "-v", mountSpec
                  , "-w", "/mount/personal-configs"
                  , convertString imgName
                  , "stack"
                  , "build"
                  , "--fast"
                  , "--allow-different-user"
                  ] "" env

-- | Serve static websites.
websites :: DevOp Env ()
websites =
    void $ reverseProxy "/opt/rundir-websites" nginxConfigs
  where
    nginxConfigs :: [DevOp env NginxServerConfig]
    nginxConfigs = [
        site "dicioccio.fr" "https://github.com/lucasdicioccio/site"
      , site "lubian.info" "https://github.com/lubian/site"
      ]
    site :: HostName -> GitUrl -> DevOp env NginxServerConfig
    site host url = StaticSite.gitCloned repo
      where
        repo = gitClone url "master" Debian.git (userDirectory (convertString host) user)
        user = mereUser "staticsites"

remoteHost :: IpNetString -> Name -> DevOp Env ControlledHost
remoteHost ip user = control user (existingRemote ip)

binPath :: FilePath
binPath =
    "./cloning-src/.stack-work/install/x86_64-linux/lts-11.8/8.2.2/bin/personal-configs-exe"
