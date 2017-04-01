{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Devops.DotNetCore where

import           GHC.TypeLits (KnownSymbol)
import           Data.Monoid ((<>))
import           Data.String.Conversions (convertString)
import           System.FilePath.Posix ((</>))

import           Devops.App
import           Devops.Base
import qualified Devops.Debian.Commands as Cmd
import           Devops.Debian.User (User, Group)
import           Devops.Service
import           Devops.Storage

restore :: AppCommand "dotnet" a
restore a = [ "restore" , getDirectoryPresentPath $ appDir a ]

build :: AppCommand "dotnet" a
build a = [ "build" , getDirectoryPresentPath $ appDir a ]

data Running x = Running x

run :: AppCommand "dotnet" a
run a =
  let d    = getDirectoryPresentPath $ appDir a
      path = d </> convertString (appName a)  <> ".csproj"
  in [ "run" , "-p", path ]

dotnetApp :: KnownSymbol x => Name -> DevOp DirectoryPresent -> DevOp (App "dotnet" x)
dotnetApp name repo = devop id mkOp $ do
    application name Cmd.dotnet repo
  where
    mkOp a = let path = convertString $ getDirectoryPresentPath (appDir a) in
            buildOp
                ("dotnet-app: " <> path)
                ("builds " <> path <> " dotnet-core application")
                noCheck
                (runApp a restore >> runApp a build)
                noAction
                noAction

dotnetRun :: KnownSymbol x
          => Name
          -> DevOp (App "dotnet" x)
          -> DevOp (Running (App "dotnet" x))
dotnetRun name mkApp = devop id mkOp $ do
    a <- mkApp
    return $ Running a
  where
    mkOp (Running a) = let path = convertString $ getDirectoryPresentPath (appDir a) in
            buildOp
                ("dotnet-run: " <> name)
                ("runs " <> path <> " dotnet-core application")
                noCheck
                (runApp a run)
                noAction
                noAction

dotnetDaemon :: KnownSymbol x
             => Name
             -> DevOp (App "dotnet" x)
             -> DevOp (User, Group)
             -> DevOp (Daemon (App "dotnet" x))
dotnetDaemon name mkApp mkUserGroup =
    daemonizeApp name mkApp run (Just mkUserGroup)
