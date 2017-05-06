{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad             (forM, mapM, void)
import           Data.Monoid
import           Data.String.Conversions   (convertString)
import           Data.Text                 (pack)
import           DepTrack
import           Devops.Base               (DevOp, noop)
import           Devops.Bootstrap.Build
import           Devops.Bootstrap.DO
import           Devops.Bootstrap.Parasite
import           Devops.Callback           (binaryCall, continueConst)
import           Devops.Cli                (App (..), Method (..), SelfPath,
                                            appMain, appMethod, methodArg,
                                            simpleMain)
import           Devops.Debian.Base        (deb)
import           Devops.Debian.Commands    (git)
import           Devops.Debian.User        (Group (..), User (..),
                                            directoryPermissions, homeDirPath,
                                            user, group, noExtraGroup, preExistingUser)
import           Devops.Git                (GitRepo (..), gitClone, gitDir)
import           Devops.Haskell            (stackPackage)
import           Devops.Optimize           (optimizeDebianPackages)
import           Devops.OS
import           Devops.Ref
import           Devops.Storage            (DirectoryPresent (..), FileContent,
                                            FileLinked, FilePresent (..),
                                            directory, fileContent, fileLink,
                                            preExistingFile, (</>))
import           Network.DO                hiding (error)
import           System.Environment        (getArgs)


-- | Stages of execution of this application
data Stage = Local String Int
           -- ^ The binary was called (e.g., by a human) on the local-host.
           | Remote
           -- ^ The binary was called remotely

parseStage :: [String] -> (Stage, Method)
parseStage = \case
    ("_remote_":arg:[]) -> (Remote, appMethod arg)
    (host:key:arg:[])   -> (Local host (read key),  appMethod arg)
    args                -> error $ "unparsed args: " ++ show args

unparseStage :: Stage -> Method -> [String]
unparseStage stage m = case stage of
    Local h k -> [ h, show k, methodArg m ]
    Remote    -> [ "_remote_", methodArg m ]

--------------------------------------------------------------------

main :: IO ()
main = do
    let app = App parseStage unparseStage stages [optimizeDebianPackages] :: App Stage
    appMain app

stages :: Stage -> SelfPath -> (Stage -> Method -> [String]) -> DevOp ()
stages Remote _ _         = devtools
stages (Local hn key)  self fixCall = void $ do
    -- prepare callbacks for binary calls
    let remoteCallback = binaryCall self (fixCall Remote)

    -- the droplet
    let droplet = parasitedHost hn key

    -- the remotely configured droplet
    remoteContinued root
      (continueConst devtools remoteCallback)
      droplet

exe               = "deptrack-devops-example-devbox"
root              = preExistingUser "root"
devGroup          = group "curry"
devUser           = user "curry" devGroup noExtraGroup
allIps            = "0.0.0.0"
dropletConfig key = standardDroplet { size = G4, configImageSlug = ubuntuXenialSlug, keys = [key] }

parasitedHost :: String -> Int -> DevOp ParasitedHost
parasitedHost dropletName key = do
  let host     = droplet False ((dropletConfig key) { configName = dropletName } )
      built    = build ubuntu16_04 ".." exe
      doref    = saveRef (pack dropletName)
      resolved = resolveRef doref host
  parasite root (buildOutput built) resolved

devtools :: DevOp ()
devtools = void $ do
        packages
        dotFiles (dotFilesDir ((,) <$> devUser <*> devGroup))

packages :: DevOp ()
packages = void $ do
    deb "git-core"
    deb "emacs"
    deb "tmux"
    deb "graphviz"
    deb "stack"

dotFilesDir :: DevOp (User, Group) -> DevOp (User, DirectoryPresent)
dotFilesDir mkUsrGrp = do
  (User u, _) <- mkUsrGrp
  let home = homeDirPath u
      dotfilesDir = directory $ home </> "dotfiles"
  d <- directoryPermissions mkUsrGrp dotfilesDir
  return (User u, d)

dotFiles :: DevOp (User, DirectoryPresent) -> DevOp [FilePresent]
dotFiles mkDirUser = do
  let u = fmap fst mkDirUser
  let d = fmap snd mkDirUser
  let repo = gitClone "https://github.com/abailly/dotfiles" "master" git d
  forM [ ".tmux.conf"
       , ".emacs"
       , ".gitconfig"
       , ".bash_profile"
       ] $ symlinkFile u repo

symlinkFile :: DevOp User -> DevOp GitRepo -> FilePath -> DevOp FilePresent
symlinkFile mkUser mkRepo filepath = do
  User u          <- mkUser
  let home = homeDirPath u
  fst <$> fileLink (home </> filepath) (preExistingFileIn mkRepo filepath)

preExistingFileIn :: DevOp GitRepo -> FilePath -> DevOp FilePresent
preExistingFileIn mkRepo fp = do
  GitRepo dir _ _ <- mkRepo
  return $ FilePresent (getDirectoryPresentPath dir </> fp)
