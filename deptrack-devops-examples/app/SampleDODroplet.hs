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

sampleDONode :: Bool -> String -> DevOp env ParasitedHost
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
  let go (host,bool) _ _ = void $ sampleDONode bool host
  let parse (host:"-v":x) = ((host,True), appMethod $ head x)
      parse (host:x)      = ((host,False), appMethod $ head x)
  let unparse _ x = [methodArg x]
  appMain $ App parse unparse go [optimizeDebianPackages] (\_ -> pure ())
