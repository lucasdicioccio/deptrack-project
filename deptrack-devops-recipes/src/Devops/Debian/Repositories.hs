{-# LANGUAGE OverloadedStrings #-}

module Devops.Debian.Repositories where

import qualified Data.ByteString.Char8   as ByteString
import           Data.Monoid             ((<>))
import           Data.String.Conversions (convertString)
import qualified Data.Text               as Text
import           System.FilePath.Posix   ((</>))

import           Devops.Debian
import           Devops.Storage.Base
import           Devops.Base

type AptGetRepositoryUrl = String

trusty,wheezy :: Name
trusty = "trusty"
wheezy = "wheezy"

fpComplete :: DevOp DebianRepository
fpComplete = do
  let url = "http://download.fpcomplete.com/debian"
  let repo = sourceRepository "fpco" url (Right (wheezy,"main"))
  let keys = aptGetKeys "keyserver.ubuntu.com" "575159689BEFB442"
  DebianRepository <$> repo <*> keys

rCran :: DevOp DebianRepository
rCran = do
  let url = "http://cran.cnr.berkeley.edu/bin/linux/ubuntu"
  let repo = sourceRepository "r-cran" url (Left $ Text.unpack $ trusty<>"/")
  let keys = aptGetKeys "keyserver.ubuntu.com" "E084DAB9"
  DebianRepository <$> repo <*> keys

docker :: DevOp DebianRepository
docker = do
  let url = "https://apt.dockerproject.org/repo"
  let repo = sourceRepository "docker" url (Right ("ubuntu-"<>trusty,"main"))
  let keys = aptGetKeys "hkp://p80.pool.sks-keyservers.net:80" "58118E89F3A912897C070ADBF76221572C52609D"
  DebianRepository <$> repo <*> keys

sourceRepository ::
     Name
  -> AptGetRepositoryUrl
  -> Either FilePath (Name,String)
  -> DevOp (FilePresent)
sourceRepository basename url distrospec =
  let lineTail = case distrospec of
             Left rpath          -> [convertString rpath]
             Right (distro,spec) -> [convertString distro, convertString spec]
      path = "/etc/apt/sources.list.d" </> (Text.unpack basename <> ".list")
      aptlines = ByteString.unwords (["deb", convertString url] <> lineTail)
      content = ByteString.unlines [ aptlines ]
  in fmap snd $ fileContent path (pure content)
