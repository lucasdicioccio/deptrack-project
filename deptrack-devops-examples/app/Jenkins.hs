{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment (getArgs)

import           Devops.Cli (defaultMain)
import           Devops.Optimize (optimizeDebianPackages)
import           Devops.Debian.Packages (jenkins)

main :: IO ()
main = getArgs >>= defaultMain jenkins [optimizeDebianPackages]
