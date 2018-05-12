{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment (getArgs)

import           Devops.Cli (simpleMain)
import           Devops.Optimize (optimizeDebianPackages)
import           Devops.Debian.Packages (jenkins)

main :: IO ()
main = do
  args <- getArgs
  simpleMain jenkins [optimizeDebianPackages] args ()
