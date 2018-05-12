{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Monad             (forM, void)
import           Data.Text                 (pack)
import           Devops.Base               (DevOp)
import           Devops.Bootstrap.Build
import           Devops.Bootstrap.DO
import           Devops.Bootstrap.Parasite
import           Devops.Callback           (binaryCall, continueConst)
import           Devops.Cli                (App (..), Method (..), SelfPath,
                                            appMain, appMethod, methodArg)
import           Devops.Debian.Base        (deb)
import           Devops.Debian.Commands    (git)
import           Devops.Debian.User        (Group (..), User (..),
                                            directoryPermissions, homeDirPath,
                                            mereUser, preExistingUser,
                                            userDirectory)
import           Devops.Git                (GitRepo (..), gitClone)
import           Devops.Optimize           (optimizeDebianPackages)
import           Devops.OS
import           Devops.Ref
import           Devops.Storage            (DirectoryPresent (..),
                                            FilePresent (..), fileLink, (</>))
import           Network.DO                hiding (error)
import           System.IO                 (BufferMode (..), hSetBuffering,
                                            stderr, stdout)


-- | Assumes a homogeneous env.
data NoEnv = NoEnv

-- | Stages of execution of this application
data Stage = Local String Int
           -- ^ The binary was called (e.g., by a human) on the local-host.
           | Remote
           -- ^ The binary was called remotely

parseStage :: [String] -> (Stage, Method)
parseStage = \case
    ("_remote_":arg:[]) -> (Remote, appMethod arg)
    (host:doKey:arg:[]) -> (Local host (read doKey),  appMethod arg)
    args                -> error $ "unparsed args: " ++ show args

unparseStage :: Stage -> Method -> [String]
unparseStage stage m = case stage of
    Local h k -> [ h, show k, methodArg m ]
    Remote    -> [ "_remote_", methodArg m ]

--------------------------------------------------------------------

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    let app = App parseStage unparseStage stages [optimizeDebianPackages] (\_ -> return NoEnv) :: App NoEnv Stage
    appMain app

stages :: Stage -> SelfPath -> (Stage -> Method -> [String]) -> DevOp NoEnv ()
stages Remote _ _         = devtools
stages (Local hn key)  self fixCall = void $ do
    -- prepare callbacks for binary calls
    let remoteCallback = binaryCall self (fixCall Remote)

    -- the droplet
    let doDroplet = parasitedHost hn key

    -- the remotely configured droplet
    remoteContinued root
        (continueConst devtools remoteCallback)
        NoEnv
        doDroplet

exe               = "deptrack-devops-example-devbox"
root              = preExistingUser "root"
devUser           = mereUser "curry"
allIps            = "0.0.0.0"
dropletConfig key = standardDroplet { size = SizeSlug "4gb", configImageSlug = ubuntuXenialSlug, keys = [key] }

parasitedHost :: String -> Int -> DevOp env ParasitedHost
parasitedHost dropletName key = do
    let host     = droplet False ((dropletConfig key) { configName = dropletName } )
        built    = build ubuntu16_04 ".." exe
        doref    = saveRef (pack dropletName)
        resolved = resolveRef doref host
    parasite root (buildOutput built) resolved

devtools :: DevOp NoEnv ()
devtools = void $ do
    packages
    dotFiles devUser (dotFilesDir devUser)

packages :: DevOp env ()
packages = void $ do
    deb "git-core"
    deb "emacs"
    deb "tmux"
    deb "graphviz"
    deb "haskell-stack"

dotFilesDir :: DevOp env User -> DevOp env DirectoryPresent
dotFilesDir mkUsr = do
    User u <- mkUsr
    let dotfilesDir = userDirectory "dotfiles" mkUsr
        userAndGroup = (,Group u) <$> mkUsr
    directoryPermissions userAndGroup dotfilesDir

dotFiles :: DevOp env User -> DevOp env DirectoryPresent -> DevOp env [FilePresent]
dotFiles mkUsr dotFiles = do
    let repo = gitClone "https://github.com/abailly/dotfiles" "master" git dotFiles
    forM [ ".tmux.conf"
         , ".emacs"
         , ".gitconfig"
         , ".bash_profile"
         ] $ symlinkFile mkUsr repo

symlinkFile :: DevOp env User -> DevOp env GitRepo -> FilePath -> DevOp env FilePresent
symlinkFile mkUser mkRepo filepath = do
    User u          <- mkUser
    let home = homeDirPath u
    fst <$> fileLink (home </> filepath) (preExistingFileIn mkRepo filepath)

preExistingFileIn :: DevOp env GitRepo -> FilePath -> DevOp env FilePresent
preExistingFileIn mkRepo fp = do
    GitRepo dir _ _ <- mkRepo
    return $ FilePresent (getDirectoryPresentPath dir </> fp)
