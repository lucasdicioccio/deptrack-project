{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Devops.Haskell (StackProject, stackProject, stackInstall, stackPackage) where

import           Control.Monad          (void)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Data.Proxy             (Proxy (..))
import           GHC.TypeLits           (KnownSymbol, Symbol, symbolVal)
import           System.FilePath        ((</>))

import           Devops.Binary          (Binary (..), HasBinary)
import           Devops.Debian          (suRunAsInDir)
import           Devops.Debian.Commands (git, stack)
import           Devops.Debian.User     (User (..), userDirectory)
import           Devops.Git             (GitBranch, GitRepo (..), GitUrl,
                                         gitClone)
import           Devops.Storage         (DirectoryPresent (..), directory)
import           Devops.Base            (DevOp, Name, noAction, noCheck, buildOp, devop)

type PackageName = Name
type TargetName = Name

data StackCommand =
    Setup
  | Update
  | Build
  | Install !PackageName
  | InstallBinaryIn !PackageName !TargetName !FilePath
  | Exec [Text]
  deriving Show

-- | A StackProject parametrized per package name.
--
-- The symbol types should correspond to the package name.
data StackProject (a :: Symbol) = StackProject {
    stackProjectDir  :: !DirectoryPresent
  , stackProjectUser :: !User
  }

stackInstall ::
     (KnownSymbol bin, KnownSymbol pkg, HasBinary (StackProject pkg) bin)
  => FilePath
  -- ^ directory where to store binaries from the project
  -> DevOp env (StackProject pkg)
  -- ^ project to build binaries from
  -> DevOp env (Binary bin)
stackInstall = go Proxy Proxy
  where
    go :: (KnownSymbol bin, KnownSymbol pkg, HasBinary (StackProject pkg) bin)
       => Proxy bin
       -> Proxy pkg
       -> FilePath
       -> DevOp env (StackProject pkg)
       -> DevOp env (Binary bin)
    go proxyBin proxyPkg installdir mkProj = do
        let bin = symbolVal proxyBin
        let b = Text.pack bin
        let pkg = symbolVal proxyPkg
        let p = Text.pack pkg
        stackRun stack
                 (stackProjectDir <$> mkProj)
                 (stackProjectUser <$> mkProj)
                 [InstallBinaryIn p b installdir]
        return $ (Binary $ installdir </> bin)

-- | Bootstraps a Stack project in a user directory as follows:
-- - clone-it
-- - set-it-up (with `stack setup`)
-- - builds it (with `stack build`)
stackProject :: GitUrl
             -> GitBranch
             -> FilePath
             -> DevOp env User
             -> DevOp env (StackProject a)
stackProject url branch installDir user = do
    let repo = getDir <$> gitClone url branch git (userDirectory installDir user)
    let allcommands = [Setup, Update, Build]
    stackRun stack repo user allcommands
    StackProject <$> repo <*> user
  where
    getDir (GitRepo d _ _) = d

-- | Installs a stack package.
stackPackage :: Name -> DevOp env User -> DevOp env ()
stackPackage n user = do
  stackRun stack (directory "/") user [Setup, Install n]

-- | Runs a list of stack commands in a directory.
stackRun :: DevOp env (Binary "stack")
         -> DevOp env DirectoryPresent
         -> DevOp env User
         -> [StackCommand]
         -> DevOp env ()
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
    Setup       -> suRunAsInDir s d u ["setup"] ""
    Build       -> suRunAsInDir s d u ["build"] ""
    Update      -> suRunAsInDir s d u ["update"] ""
    (Install n) -> suRunAsInDir s d u ["install", Text.unpack n] ""
    (InstallBinaryIn p b path) -> do
        let tgt = Text.unpack $ p <> ":" <> b
        let args = [ "install" , "--allow-different-user"
                   , "--local-bin-path=" <> path
                   , tgt
                   ]
        suRunAsInDir s d u args ""
    (Exec args) -> suRunAsInDir s d u ("exec":"--":(fmap Text.unpack args)) ""
