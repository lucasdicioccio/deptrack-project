{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import           System.Environment (getArgs)
import           System.Environment (getArgs)

import           Devops.Cli (defaultMain)
import           Devops.Optimize (optimizeDebianPackages)

import           GHC.TypeLits
import           DepTrack
import           Devops.App
import           Devops.DotNetCore
import           Devops.Base
import           Devops.Service
import           Devops.Git
import           Devops.Debian.User
import qualified Devops.Debian.Commands as Cmd

main :: IO ()
main = getArgs >>= defaultMain helloDaemon [optimizeDebianPackages]

daemonGroup :: DevOp Group
daemonGroup = group "user"

daemonUser :: DevOp User
daemonUser = user "user" daemonGroup noExtraGroup

helloDaemon :: DevOp (Daemon (App "dotnet" "hello-world"))
helloDaemon = let usrgrp = (,) <$> daemonUser <*> daemonGroup in
    dotnetDaemon "dotnet-daemon-example" helloApp usrgrp

helloURL :: GitUrl
helloURL = "https://github.com/microservices-aspnetcore/hello-world.git"

helloApp :: DevOp (App "dotnet" "hello-world")
helloApp =
    dotnetApp "hello-world" (gitDir <$> repo)
  where
    repo = gitClone helloURL "master" Cmd.git (userDirectory "dotnet-xyz" daemonUser)