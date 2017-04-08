{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Functor
import           Data.Monoid
import           Data.Text                 (pack)
import           DepTrack
import           Devops.Actions
import           Devops.Base
import           Devops.Bootstrap
import           Devops.Bootstrap.Build
import           Devops.Bootstrap.DO
import           Devops.Bootstrap.Parasite
import           Devops.Cli
import           Devops.Debian.User
import           Devops.Optimize           (optimizeDebianPackages)
import           Devops.OS
import           Devops.Ref
import           Network.DO
import           System.Environment        (getArgs)

sampleDONode :: String -> DevOp ParasitedHost
sampleDONode dropletName =
  let exe      = "deptrack-devops-example-do-droplet"
      host     = droplet (standardDroplet { configName = dropletName, keys = [429079] })
      root     = preExistingUser "root"
      built    = build ubuntu14_04 ".." exe
      doref    = saveRef (pack dropletName)
      resolved = resolveRef doref host
      infect   = parasite root (buildOutput built) resolved
  in infect

main :: IO ()
main = do
  host:args <- getArgs
  defaultMain (sampleDONode  host)  [optimizeDebianPackages] args

