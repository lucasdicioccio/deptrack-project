{-# LANGUAGE OverloadedStrings #-}

module Devops.Linux where

import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text

import           Devops.Debian.Commands
import           Devops.Base
import           Devops.Utils

type Value = Text
data KernelModule = KernelModule !Name [(Name, Value)]

-- | Loads a kernel module.
kernelmodule :: Name -> [(Name, Value)]-> DevOp KernelModule
kernelmodule n kvs = devop fst mkOp $ do
  mp <- modprobe
  return $ (KernelModule n kvs, mp)
  where
    mkOp (KernelModule n kvs, mp) =
             let args = fmap (\(k,v) -> Text.unpack $ k <> "=" <> v) kvs in
             buildOp ("kernel-module: " <> n)
                     ("adds a kernel module with modprobe")
                     noCheck
                     (blindRun mp ((Text.unpack n):args) "")
                     (blindRun mp ["-r", Text.unpack n] "")
                     noAction
