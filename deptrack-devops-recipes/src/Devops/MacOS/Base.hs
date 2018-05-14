{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Devops.MacOS.Base (
    HomebrewPackage(..)
  , homebrewPackage
  , brew
  , installedWith
  , xcodeSelect
  ) where

import           Data.Monoid    ((<>))
import           Data.Proxy     (Proxy (..))
import qualified Data.Text      as Text
import           GHC.TypeLits   (KnownSymbol, Symbol, symbolVal)

import           Devops.Binary
import           Devops.Base
import           Devops.Utils

data HomebrewPackage (a :: Symbol) = HomebrewPackage !Name

installedWith
  :: DevOp env (Binary c)
  -> DevOp env (HomebrewPackage a)
  -> DevOp env (Binary c)
b `installedWith` pkg = pkg *> b -- works because binary is generally pure

homebrewPackage :: (KnownSymbol a) => DevOp env (HomebrewPackage a)
homebrewPackage = f Proxy
  where
    f :: (KnownSymbol a) => Proxy a -> DevOp env (HomebrewPackage a)
    f proxy = brew (Text.pack (symbolVal proxy))

homebrew :: DevOp env (Binary "brew")
homebrew = binary

xcodeSelect :: DevOp env (Binary "xcode-select")
xcodeSelect = binary

brew :: Name -> DevOp env (HomebrewPackage a)
brew n = fmap snd $ track mkOp $ do
    brewBin <- homebrew
    return (brewBin, HomebrewPackage n)
  where
    mkOp (b,_) = buildPreOp
        ("homebrew-package: " <> n)
        ("ensure that " <> n <> " is installed.")
        (checkOp b)
        (up b)
        (down b)
        noAction
    checkOp b = checkBinaryExitCodeAndStdout
        (elem (Text.unpack n) . lines)
        b
        ["list"]
        ""
    up b = do
        blindRun b ["update"] ""
        blindRun b (["install"] ++ [Text.unpack n]) ""
    down b =
        blindRun b (["uninstall"] ++ [Text.unpack n]) ""
