{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE OverloadedStrings #-}

module Devops.Haskell (StackProject, stackProject, stackInstall, stackPackage) where

import           Control.Monad          (void)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Proxy             (Proxy (..))
import           GHC.TypeLits           (KnownSymbol, Symbol, symbolVal)
import           System.FilePath        ((</>))

import           DepTrack               (track)
import           Devops.Binary          (Binary (..), HasBinary)
import           Devops.Debian          (sudoRunAsInDir)
import           Devops.Debian.Commands (git, stack)
import           Devops.Debian.User     (User (..), userDirectory)
import           Devops.Git             (GitBranch, GitRepo (..), GitUrl,
                                         gitClone)
import           Devops.Storage         (DirectoryPresent (..), directory)
import           Devops.Base            (DevOp, Name, noAction, noCheck, buildOp, devop)

data StackCommand = Setup | Update | Build | Install !Name | InstallIn !Name !FilePath | Exec [Text]
  deriving Show

data StackProject (a :: Symbol) = StackProject DirectoryPresent User

stackInstall :: (KnownSymbol bin, HasBinary (StackProject pkg) bin) =>
  FilePath -> DevOp (StackProject pkg) -> DevOp (Binary bin)
stackInstall = go Proxy
  where
    go :: (KnownSymbol bin, HasBinary (StackProject pkg) bin) =>
      Proxy bin -> FilePath -> DevOp (StackProject pkg) -> DevOp (Binary bin)
    go proxy installdir mkProj = do
        let bin = symbolVal proxy
        stackRun stack (projdir <$> mkProj) (projuser <$> mkProj) [InstallIn (Text.pack bin) installdir]
        return $ (Binary $ installdir </> bin)

    projdir (StackProject d _) = d
    projuser (StackProject _ u) = u


-- | Bootstraps a Stack project in a user directory as follows:
-- - clone-it
-- - set-it-up (with `stack setup`)
-- - builds it (with `stack build`)
stackProject :: GitUrl
             -> GitBranch
             -> FilePath
             -> DevOp User
             -> DevOp (StackProject a)
stackProject url branch installDir user = do
    let repo = getDir <$> gitClone url branch git (userDirectory installDir user)
    let allcommands = [Setup, Update, Build]
    stackRun stack repo user allcommands
    StackProject <$> repo <*> user
  where
    getDir (GitRepo d _ _) = d

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
          ("stack-run: " <> Text.pack d)
          ("stack run commands in project at " <> Text.pack d <> Text.pack (show commands))
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
    (InstallIn n path) -> sudoRunAsInDir s d (u,u) ["install", "--allow-different-user", "--local-bin-path=" <> path, Text.unpack n] ""
    (Exec args) -> sudoRunAsInDir s d (u,u) ("exec":"--":(fmap Text.unpack args)) ""
