{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Control.Monad (void)
import           Data.String.Conversions (convertString)
import           System.Environment (getArgs)
import           Devops.Binary (Binary, HasBinary)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Monoid ((<>))

import           DepTrack (inject)
import           Devops.Debian.Base (DebianPackage, debianPackage, deb, installedWith)
import           Devops.Storage (FilePresent(..), FileContent, fileContent)
import           Devops.Haskell (StackProject, stackProject, stackInstall)
import           Devops.Debian.User (mereUser, group)
import           Devops.Base (DevOp, buildOp, devop, noAction, noCheck)
import           Devops.Cli (simpleMain)
import           Devops.Optimize (optimizeDebianPackages)
import           Devops.Postgre (PGUser(..), PGDatabase(..), pgUser, pgDatabase, libpqDev)
import           Devops.Service (daemon, Daemon, CommandArgs, DaemonConfig)

main :: IO ()
main = getArgs >>= simpleMain devtools [optimizeDebianPackages]
  where
    devtools :: DevOp ()
    devtools = void $ do
        let db = pgDatabase "gloubyboulga" (pgUser "casimir" "lileauxenfantsglauquissime")
        let cfg = fileContent "/home/user/postgrest.cfg" (convertString . postgrestconfig <$> db)
        postgrestService $ fmap snd cfg

data Postgrest
type instance DaemonConfig Postgrest = FilePresent

postgrestCommandArgs :: DaemonConfig Postgrest -> CommandArgs
postgrestCommandArgs (FilePresent path) = [path]

postgrestService :: DevOp FilePresent -> DevOp (Daemon Postgrest)
postgrestService fp =
    daemon "postgrest" (Just ((,) <$> mereUser "user" <*> group "user")) (postgrest "/home/user/.local/bin") postgrestCommandArgs fp


postgrestProject :: DevOp (StackProject "postgrest")
postgrestProject = fmap fst $ do
    let url = "https://github.com/begriffs/postgrest.git" 
    let branch = "master"
    let systemDependencies = libpqDev
    stackProject url branch "postgrest" (mereUser "user") `inject` systemDependencies

instance HasBinary (StackProject "postgrest") "postgrest" where

postgrest :: FilePath -> DevOp (Binary "postgrest")
postgrest installdir = stackInstall installdir postgrestProject 

postgrestconfig :: PGDatabase -> String
postgrestconfig (PGDatabase dbname (PGUser user pass)) = unlines [
    "db-uri = \"postgres://" <> Text.unpack user <> ":" <> Text.unpack pass <> "@localhost:5432/" <> Text.unpack dbname <> "\""
  , "db-schema = \"public\""
  , "db-anon-role = \"" <> Text.unpack user <> "\""
  , "db-pool = 10"
  , "server-host = \"*4\""
  , "server-port = 3000"
  ]
