{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Configs where

import           Control.Monad (void)
import           Data.Monoid ((<>))
import           Data.String.Conversions (convertString)
import           Devops.Base (DevOp, devop, buildOp, noAction, noCheck, Name)
import           Devops.Cli (App (..), Method (..), SelfPath, appMain, appMethod, methodArg)
import           Devops.Constraints (HasOS(..), onOS)
import           Devops.Docker (DockerImage(..), pulledDockerImage, resolveDockerEnv)
import           Devops.Git (GitRepo(..), gitCloneSimple)
import           Devops.Optimize (optimizeDebianPackages)
import           Devops.Ref
import           Devops.Storage (DirectoryPresent(..), directory)
import           Devops.MacOS.Base (brew)
import qualified Devops.MacOS.Commands as MacOS
import           Devops.Utils (blindRunWithEnv)
import           System.IO  (BufferMode (..), hSetBuffering, stderr, stdout)
import           System.Directory (makeAbsolute)

-- | We configure MacbookAir only for now.
data Env = MacbookAir
instance HasOS Env where
    os MacbookAir = "mac-os"

discoverEnv :: IO Env
discoverEnv = pure MacbookAir

-- | We configure MacbookAir only for now.
data Stage = Laptop

parseStage :: [String] -> (Stage, Method)
parseStage = \case
    (arg:[]) ->
        (Laptop, appMethod arg)
    args                ->
        error $ "unparsed args: " ++ show args

unparseStage :: Stage -> Method -> [String]
unparseStage stage m = case stage of
    Laptop -> [ "macbookd-air", methodArg m ]

runMain :: IO ()
runMain = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    let app = App parseStage unparseStage stages [optimizeDebianPackages] (const $ discoverEnv)
    appMain app

stages :: Stage -> SelfPath -> (Stage -> Method -> [String]) -> DevOp Env ()
stages Laptop _ _         = macbookAir

-- | Setup enough for developing on my Macbook air.
macbookAir :: DevOp Env ()
macbookAir = onOS "mac-os" $ void $ do
    brew "tree"
    brew "tmux"
    MacOS.dot
    let fpcoImg = pulledDockerImage "fpco/stack-build"
    let cloneUrl = "https://github.com/lucasdicioccio/deptrack-project.git" 
    let mappedDir = gitDir <$> gitCloneSimple cloneUrl "master" MacOS.git (directory "cloning-src")
    crossCompile "x-compile" fpcoImg mappedDir

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
