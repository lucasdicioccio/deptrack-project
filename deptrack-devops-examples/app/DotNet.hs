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
import           Devops.Git
import           Devops.Debian.User
import qualified Devops.Debian.Commands as Cmd

main :: IO ()
main = getArgs >>= defaultMain (dotnetRun foo) [optimizeDebianPackages]

foo :: DevOp (App "dotnet" "dotnet-xxx")
foo = dotnetApp (gitDir <$> repo)
  where
    repo = gitClone "https://github.com/microservices-aspnetcore/hello-world.git" "master" Cmd.git (userDirectory "dotnet-xxx" (mereUser "user"))

