{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Devops.Git where

import           Data.Monoid             ((<>))
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           System.Posix.Files      (fileGroup, fileOwner, getFileStatus)
import           System.Posix.User       (getGroupEntryForID, getUserEntryForID,
                                          groupName, userName)

import           Devops.Base
import           Devops.Binary
import           Devops.Debian (sudoRunAsInDir)
import           Devops.Storage
import           Devops.Utils (blindRun)

type GitUrl = Text
type GitBranch = Text
type GitPath = Text
data GitRepo =
    GitRepo { gitDir      :: !DirectoryPresent
            , gitRemote   :: !GitPath
            , gitRepoName :: !Name
            } deriving Show

gitClone :: GitPath -> GitBranch -> DevOp env (Binary "git") -> DevOp env DirectoryPresent -> DevOp env GitRepo
gitClone path branch mkGit mkDir = devop snd mkOp $ do
  dir <- mkDir
  g <- mkGit
  return (g,(GitRepo dir path branch))
  where mkOp (g,(GitRepo (DirectoryPresent d) r b)) = buildOp
          ("git-repo: " <> convertString d)
          ("clones " <> r <> " at " <> convertString d)
          noCheck
          (cloneInUserDirectoryRespectingOwner g d b r)
          (noAction)
          (noAction)

        cloneInUserDirectoryRespectingOwner g d b r = do
            status <- getFileStatus d
            username <- Text.pack . userName <$> getUserEntryForID (fileOwner status)
            groupname <- Text.pack . groupName <$> getGroupEntryForID (fileGroup status)
            let run = sudoRunAsInDir g d (username,groupname)
            run ["clone", "-b", Text.unpack b, Text.unpack r, d] ""
            run ["pull", "origin", Text.unpack b] ""
            run ["submodule", "init"] ""
            run ["submodule", "update"] ""

gitCloneSimple
  :: GitPath
  -> GitBranch
  -> DevOp env (Binary "git")
  -> DevOp env DirectoryPresent
  -> DevOp env GitRepo
gitCloneSimple path branch mkGit mkDir = devop snd mkOp $ do
  dir <- mkDir
  g <- mkGit
  return (g,(GitRepo dir path branch))
  where mkOp (g,(GitRepo (DirectoryPresent d) r b)) = buildOp
          ("git-repo: " <> convertString d)
          ("clones " <> r <> " at " <> convertString d)
          noCheck
          (clone g d b r)
          (noAction)
          (noAction)

        clone g d b r = do
            let run = blindRun g
            run ["clone", "-b", Text.unpack b, Text.unpack r, d] ""
            run ["pull", "origin", Text.unpack b] ""
            run ["submodule", "init"] ""
            run ["submodule", "update"] ""
