{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Devops.DotNetCore where

import           GHC.TypeLits (KnownSymbol)
import           Data.Monoid ((<>))
import           Data.String.Conversions (convertString)

import qualified Devops.Debian.Commands as Cmd
import           Devops.App
import           Devops.Base
import           Devops.Storage

restore :: AppCommand a
restore _ = [ "restore" ]

build :: AppCommand a
build _ = [ "build" ]

data Running x = Running x
run :: AppCommand a
run _ = [ "run" ]

dotnetApp :: KnownSymbol x => DevOp DirectoryPresent -> DevOp (App "dotnet" x)
dotnetApp repo = devop id mkOp $ do
    application Cmd.dotnet `installedIn` repo
  where
    x `installedIn` y = y *> x
    mkOp a = buildOp
            ("dotnet-app: " <> convertString (appPath a))
            ("builds " <> convertString (appPath a) <> " dotnet-core application")
            noCheck
            (runApp a restore >> runApp a build)
            noAction
            noAction

dotnetRun :: KnownSymbol x
          => DevOp (App "dotnet" x)
          -> DevOp (Running (App "dotnet" x))
dotnetRun mkApp = devop id mkOp $ do
    a <- mkApp
    return $ Running a
  where
    mkOp (Running a) = buildOp
            ("dotnet-run: " <> convertString (appPath a))
            ("runs " <> convertString (appPath a) <> " dotnet-core application")
            noCheck
            (runApp a run)
            noAction
            noAction
