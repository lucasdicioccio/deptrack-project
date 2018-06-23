{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import           Control.Monad (void)

import           Devops.Cli (App(..), appMain, appMethod, methodArg)
import           Devops.Optimize (optimizeDebianPackages)

import           GHC.TypeLits
import           DepTrack
import qualified Devops.App as PlatformApp
import           Devops.DotNetCore
import           Devops.Base
import           Devops.Service
import           Devops.Git
import           Devops.Debian.User
import qualified Devops.Debian.Commands as Cmd

main :: IO ()
main = do
    let go _ _ _ = void $ helloDaemon
    let parse x = ((), appMethod $ head x)
    let unparse _ x = [methodArg x]
    appMain $ App parse unparse go [optimizeDebianPackages] (\_ -> pure ())

daemonGroup :: DevOp env Group
daemonGroup = group "user"

daemonUser :: DevOp env User
daemonUser = user "user" daemonGroup noExtraGroup

helloDaemon :: DevOp env (Daemon (PlatformApp.App "dotnet" "hello-world"))
helloDaemon = let usrgrp = (,) <$> daemonUser <*> daemonGroup in
    dotnetDaemon "dotnet-daemon-example" helloApp usrgrp

helloURL :: GitUrl
helloURL = "https://github.com/microservices-aspnetcore/hello-world.git"

helloApp :: DevOp env (PlatformApp.App "dotnet" "hello-world")
helloApp =
    dotnetApp "hello-world" (gitDir <$> repo)
  where
    repo = gitClone helloURL "master" Cmd.git (userDirectory "dotnet-xyz" daemonUser)
