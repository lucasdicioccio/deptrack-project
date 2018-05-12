{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Devops.R where

import           Data.Monoid            ((<>))
import qualified Data.Text              as Text

import           Devops.Debian.Commands
import           Devops.Downloads
import           Devops.Base
import           Devops.Utils

data RPackage = RPackage !Name

rPackage :: Name -> DevOp env DownloadedFile -> DevOp env (RPackage)
rPackage n dlPkg = devop fst mkOp $ do
    b <- r
    (DownloadedFile _ path) <- dlPkg
    return (RPackage n, (path,b))
  where
    mkOp (_, (path, b)) = buildOp
               ("r-package: " <> n)
               ("R installs the following package: " <> n)
               noCheck
               (blindRun b ["CMD", "INSTALL", path] "")
               noAction
               noAction

rTgzPkg :: URLString -> Name -> String -> DevOp env RPackage
rTgzPkg repo name version = do
  let url = repo <> Text.unpack name <> "_" <> version <> ".tar.gz"
  let path = Text.unpack name
  rPackage name (download url path)

rPkg :: URLString -> Name -> DevOp env RPackage
rPkg url pkgname = devop fst mkOp $ do
    b <- r
    return (RPackage pkgname, b)
  where
    mkOp (RPackage n, b) = buildOp
               ("r-package: " <> n)
               ("R downloads the following package: " <> n)
               noCheck
               (blindRun b ["CMD", "BATCH", "/dev/stdin"] rscript)
               noAction
               noAction
    rscript = "install.packages(\""<>Text.unpack pkgname<>"\",repos=\""<>url<>"\")"

