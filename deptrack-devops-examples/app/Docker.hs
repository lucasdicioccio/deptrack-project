{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment (getArgs)

import           Devops.Base (DevOp)
import qualified Devops.Debian.Commands as Cmd
import           Devops.Cli (defaultMain)
import           Devops.Optimize (optimizeDebianPackages)

main :: IO ()
main = do
    args <- getArgs
    defaultMain (Cmd.docker) [optimizeDebianPackages] args
