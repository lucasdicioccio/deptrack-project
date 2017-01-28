{-# LANGUAGE OverloadedStrings #-}

-- | Module to reload Apparmor profiles.
module Devops.Apparmor where

import           Data.Monoid             ((<>))
import qualified Data.Text               as Text

import           Devops.Debian.Commands
import           Devops.Storage
import           Devops.Base
import           Devops.Utils

data ApparmorProfile = ApparmorProfile !FilePresent

reloadApparmor :: DevOp FilePresent -> DevOp ApparmorProfile
reloadApparmor profileConfig = devop fst mkOp $ do
    ap <- apparmorParser
    fp@(FilePresent path) <- profileConfig
    return (ApparmorProfile fp, (ap, path))
  where
    mkOp (_, (ap, path)) = let reload = blindRun ap ["-r", path] ""
           in buildOp ("apparmor-profile: " <> Text.pack path)
                      ("configures and reload a profile")
                      noCheck
                      reload
                      noAction
                      reload

