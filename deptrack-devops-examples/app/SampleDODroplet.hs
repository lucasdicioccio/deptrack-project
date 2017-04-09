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

theDroplet dropletName = standardDroplet { configName = dropletName, keys = [2118791] }

sampleDONode :: Bool -> String -> DevOp ParasitedHost
sampleDONode debug dropletName =
  let exe      = "deptrack-devops-example-do-droplet"
      host     = droplet debug $ theDroplet dropletName
      root     = preExistingUser "root"
      built    = build ubuntu14_04 ".." exe
      doref    = saveRef (pack dropletName)
      resolved = resolveRef doref host
      infect   = parasite root (buildOutput built) resolved
  in infect

main :: IO ()
main = do
  host:args <- getArgs
  case args of
    "-v":args' -> defaultMain (sampleDONode True host)  [optimizeDebianPackages] args'
    args'      -> defaultMain (sampleDONode False host)  [optimizeDebianPackages] args'

