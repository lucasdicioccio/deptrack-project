{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Devops.Debian.Base (
    DebianPackagesSet(..)
  , aptGetKeys , DebianRepository (..)
  , DebianPackage (..) , debianPackage , deb , generalDebianPackage , installedWith
  -- experimental
  , postInstallHook
  , sudoRunAsInDir
  ) where

import           Data.Monoid    ((<>))
import           Data.Proxy     (Proxy (..))
import           Data.Set       (Set)
import qualified Data.Set       as Set
import           Data.Text      (Text)
import qualified Data.Text      as Text
import           DepTrack       (declare, track)
import           GHC.TypeLits   (KnownSymbol, Symbol, symbolVal)

import           Devops.Binary
import           Devops.Storage.Base
import           Devops.Base
import           Devops.Utils

data DebianPackage (a :: Symbol) = DebianPackage !Name
newtype DebianPackagesSet = DebianPackagesSet (Set Name)
  deriving Monoid

installedWith :: HasBinary (DebianPackage a) c =>
  DevOp (Binary c) -> DevOp (DebianPackage a) -> DevOp (Binary c)
b `installedWith` pkg = pkg *> b -- works because binary is generally pure

-- | Note that apt cannot be bootstrapped easily and hence is a leaf
-- dependency.
apt :: DevOp (DebianPackage "apt")
apt = return $ DebianPackage "apt"

instance HasBinary (DebianPackage "apt") "apt-get" where
instance HasBinary (DebianPackage "apt") "apt-key" where

debianPackage :: (KnownSymbol a) => DevOp (DebianPackage a)
debianPackage = f Proxy
  where f :: (KnownSymbol a) => Proxy a -> DevOp (DebianPackage a)
        f proxy = deb (Text.pack (symbolVal proxy))

generalDebianPackage :: Name -> DevOp DebianRepository -> DevOp (DebianPackage a)
generalDebianPackage n repo = fmap snd $ track mkOp $ do
        aptGet <- binary `installedWith` apt :: DevOp (Binary "apt-get")
        _ <- repo
        return (aptGet, DebianPackage n)
  where mkOp (b,_) = rawpreop (DebianPackagesSet $ Set.singleton n)
                           (opFromPackagesSet b)

deb :: Name -> DevOp (DebianPackage a)
deb n = fmap snd $ track mkOp $ do
        aptGet <- binary `installedWith` apt :: DevOp (Binary "apt-get")
        return (aptGet, DebianPackage n)
  where mkOp (b,_) = rawpreop (DebianPackagesSet $ Set.singleton n)
                           (opFromPackagesSet b)

opFromPackagesSet :: Binary "apt-get" -> DebianPackagesSet -> Op
opFromPackagesSet b (DebianPackagesSet pkgs) =
    let xs = Set.toList pkgs
        ys = fmap Text.unpack xs
        listStr = Text.intercalate "," xs
    in buildOp ("debian-packages: " <> listStr)
             ("ensures that " <> listStr <> " are installed.")
             (checkExitCodeAndStdout ((>1) . length . words)
                                     "dpkg-query"
                                     (["--show"] ++ ys)
                                     "")
             (blindRun b ["update"] ""
              >> blindRunWithEnv b (["install", "-y", "-q"] ++ ys) "" [("DEBIAN_FRONTEND", "noninteractive")])
             (blindRun b (["remove", "-q"] ++ ys) "")
             (noAction)

data DebianRepository = DebianRepository FilePresent AptGetKey

type KeyID = Text
type KeyServerHostName = Text
type AptGetKey = KeyID

sudoBin :: Binary "sudo"
sudoBin = bin "/usr/bin/sudo"

-- todo: DirectoryPresent, use User, Group
sudoRunAsInDir :: Binary x -> FilePath -> (Name, Name) -> [String] -> String -> IO ()
sudoRunAsInDir (Binary x) dir (user,group) args input =
  blindRunInDir sudoBin dir (["-E", "-g", Text.unpack group, "-u", Text.unpack user , "--", x] ++ args) input

aptGetKeys :: KeyServerHostName -> KeyID -> DevOp (AptGetKey)
aptGetKeys hostname fingerprint = devop snd mkOp $ do
    aptKey <- binary `installedWith` apt :: DevOp (Binary "apt-key")
    return (aptKey, fingerprint)
  where
    mkOp (b,_) = buildOp ("apt-keys: " <> fingerprint)
                         ("install keys for fingerprint:" <> fingerprint)
                         noCheck
                         (blindRun b ["adv", "--keyserver", Text.unpack hostname, "--recv-keys", Text.unpack fingerprint] "")
                         (blindRun b ["del", Text.unpack fingerprint] "")
                         noAction

postInstallHook :: PreOp -> DevOp (DebianPackage a) -> DevOp (DebianPackage a)
postInstallHook o x = declare o x
