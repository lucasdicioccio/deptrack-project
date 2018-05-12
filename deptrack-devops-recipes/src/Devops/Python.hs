{-# LANGUAGE OverloadedStrings #-}

module Devops.Python where

import           Data.Monoid            ((<>))
import qualified Data.Text              as Text

import           Devops.Debian.Commands (pip3)
import           Devops.Base           (DevOp, Name, noAction, noCheck, devop, buildOp)
import           Devops.Utils           (blindRun)

data PythonPackage = PythonPackage !Name

python3Package :: Name -> DevOp env (PythonPackage)
python3Package n = devop fst mkOp $ do
    b <- pip3
    return (PythonPackage n,b)
  where
    mkOp (PythonPackage _, b) = buildOp
                     ("pip-package: " <> n)
                     ("pip installs the following package: " <> n)
                     noCheck
                     (blindRun b ["install", Text.unpack n] "")
                     (noAction)
                     (noAction)
