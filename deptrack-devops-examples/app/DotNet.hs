{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment (getArgs)

import           Devops.Cli (defaultMain)
import           Devops.Optimize (optimizeDebianPackages)
import           Devops.Debian.Commands (dotnet)

main :: IO ()
main = getArgs >>= defaultMain dotnet [optimizeDebianPackages]
