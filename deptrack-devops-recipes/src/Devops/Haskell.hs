{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE OverloadedStrings #-}

module Devops.Haskell (stackProject, stackPackage, StackCommand(..), builtWith) where

import           Control.Monad          (void)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           GHC.TypeLits           (Symbol)

import           DepTrack               (track)
import           Devops.Binary          (Binary, HasBinary)
import           Devops.Debian          (sudoRunAsInDir)
import           Devops.Debian.Commands (git, stack)
import           Devops.Debian.User     (User (..), userDirectory)
import           Devops.Git             (GitBranch, GitRepo (..), GitUrl,
                                         gitClone)
import           Devops.Storage         (DirectoryPresent (..), directory)
import           Devops.Base            (DevOp, Name, noAction, noCheck, buildOp, devop)

data StackCommand = Setup | Update | Build | Install !Name | InstallIn !Name !FilePath | Exec [Text]

data StackProject (a :: Symbol) = StackProject

instance HasBinary (StackProject x) y where
builtWith :: HasBinary (StackProject x) c =>
    DevOp (Binary c) -> DevOp (StackProject x) -> DevOp (Binary c)
b `builtWith` pkg = pkg *> b

-- | Bootstraps a Stack project in a user directory as follows:
-- - clone-it
-- - set-it-up (with `stack setup`)
-- - builds it (with `stack build`)
-- - executes stack commands
stackProject :: GitUrl
             -> GitBranch
             -> FilePath
             -> [StackCommand]
             -> DevOp User
             -> DevOp (StackProject a)
stackProject url branch installDir commands user = do
  let repo = gitClone url branch git (userDirectory installDir user)
  let allcommands = Setup:Update:Build:commands
  stackRun stack (fmap getDir repo) user allcommands
  return StackProject
  where getDir (GitRepo d _ _) = d

-- | Installs a stack package.
stackPackage :: Name -> DevOp User -> DevOp ()
stackPackage n user = do
  stackRun stack (directory "/") user [Setup,Install n]

-- | Runs a list of stack commands in a directory.
stackRun :: DevOp (Binary "stack")
         -> DevOp DirectoryPresent
         -> DevOp User
         -> [StackCommand]
         -> DevOp ()
stackRun mkStack mkRepo mkUser commands = devop (const ()) mkOp $ do
    s <- mkStack
    r <- mkRepo
    u <- mkUser
    return (s,r,u)
  where
    mkOp (s, (DirectoryPresent d), u) = buildOp
          ("stack-install-project: " <> Text.pack d)
          ("stack install project at " <> Text.pack d)
          noCheck
          (void $ traverse (runStack s d u) commands)
          (noAction)
          (noAction)

-- | Helper to run a command as a given user using sudo underneath.
runStack :: Binary x -> FilePath -> User -> StackCommand -> IO ()
runStack s d (User u) =
  \case
    Setup       -> sudoRunAsInDir s d (u,u) ["setup"] ""
    Build       -> sudoRunAsInDir s d (u,u) ["build"] ""
    Update      -> sudoRunAsInDir s d (u,u) ["update"] ""
    (Install n) -> sudoRunAsInDir s d (u,u) ["install", Text.unpack n] ""
    (InstallIn n path) -> sudoRunAsInDir s d (u,u) ["install", "--local-bin-path=" <> path, Text.unpack n] ""
    (Exec args) -> sudoRunAsInDir s d (u,u) ("exec":"--":(fmap Text.unpack args)) ""
