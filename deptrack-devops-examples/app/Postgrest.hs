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
import           Devops.Binary (binary)
import           Devops.Debian.Base (DebianPackage, debianPackage, deb, installedWith)
import           Devops.Storage (FilePresent(..), FileContent, fileContent)
import           Devops.Haskell (StackProject, stackProject, builtWith)
import           Devops.Debian.User (mereUser)
import           Devops.Base (DevOp, buildOp, devop, noAction, noCheck)
import           Devops.Cli (defaultMain)
import           Devops.Optimize (optimizeDebianPackages)
import           Devops.Service (daemon, Daemon, CommandArgs, DaemonConfig)

import           Devops.Debian (sudoRunAsInDir)

main :: IO ()
main = getArgs >>= defaultMain devtools [optimizeDebianPackages]
  where
    devtools :: DevOp ()
    devtools = void $ do
        let db = pgDatabase "yooo" (pgUser "yoyoma" "yoyomapassword")
        let cfg = fileContent "postgrest.cfg" (convertString . postgrestconfig <$> db)
        postgrestService $ fmap snd cfg

data Postgrest
type instance DaemonConfig Postgrest = FilePresent

postgrestCommandArgs :: DaemonConfig Postgrest -> CommandArgs
postgrestCommandArgs (FilePresent path) = [path]

postgrestService :: DevOp FilePresent -> DevOp (Daemon Postgrest)
postgrestService fp =
    daemon "postgrest" Nothing postgrest postgrestCommandArgs fp

postgrestProject :: DevOp (StackProject "postgrest")
postgrestProject = fmap fst $ do
    let url = "https://github.com/begriffs/postgrest.git" 
    let branch = "master"
    let systemDependencies = deb "libpq-dev"
    stackProject url branch "postgrest" [] (mereUser "user") `inject` systemDependencies

instance HasBinary (StackProject "postgrest") "postgrest" where

postgrest :: DevOp (Binary "postgrest")
postgrest = binary `builtWith` postgrestProject

instance HasBinary (DebianPackage "postgresql") "createdb" where
instance HasBinary (DebianPackage "postgresql") "dropdb" where
instance HasBinary (DebianPackage "postgresql") "createuser" where
instance HasBinary (DebianPackage "postgresql") "dropuser" where
instance HasBinary (DebianPackage "postgresql") "psql" where

postgresql :: DevOp (DebianPackage "postgresql")
postgresql = debianPackage

psql :: DevOp (Binary "psql")
psql = binary `installedWith` postgresql

createdb :: DevOp (Binary "createdb")
createdb = binary `installedWith` postgresql

dropdb :: DevOp (Binary "dropdb")
dropdb = binary `installedWith` postgresql

createuser :: DevOp (Binary "createuser")
createuser = binary `installedWith` postgresql

dropuser :: DevOp (Binary "dropuser")
dropuser = binary `installedWith` postgresql

data PGUser = PGUser !Text !Text
data PGDatabase = PGDatabase !Text !PGUser

pgDatabase :: Text -> DevOp PGUser -> DevOp PGDatabase
pgDatabase n mkU = devop snd mkOp $ do
    user@(PGUser u _) <- mkU
    c <- createdb
    d <- dropdb
    return $ ((u,c,d), PGDatabase n user)
  where
    mkOp ((u,create,drop),_) =
        buildOp ("postgres-database: " <> n)
                ("creates a PostGre DB owned by user" <> u)
                noCheck
                (sudoRunAsInDir create "/var/lib/postgresql" ("postgres","postgres") [ "-e", "-O", Text.unpack u, Text.unpack n] "")
                (sudoRunAsInDir drop "/var/lib/postgresql" ("postgres","postgres") ["-e", Text.unpack n] "")
                noAction
    
pgUser :: Text -> Text -> DevOp PGUser
pgUser username pass = devop snd mkOp $ do
    c <- createuser
    s <- psql
    d <- dropuser
    return ((c,d,s), PGUser username pass)
  where
    mkOp ((create,drop,shell),_) =
        buildOp ("postgres-user: " <> username)
                ("creates a PostGre user")
                noCheck
                (sudoRunAsInDir create "/var/lib/postgresql" ("postgres","postgres") ["-e", Text.unpack username, ""] ""
                 >> superPsqlShell shell ("ALTER USER " <> Text.unpack username <> " WITH PASSWORD '" <> Text.unpack pass <> "';"))
                (sudoRunAsInDir drop "/var/lib/postgresql" ("postgres","postgres") ["-e", Text.unpack username] "")
                noAction

superPsqlShell :: Binary "psql" -> String -> IO ()
superPsqlShell shell sql =
    sudoRunAsInDir shell "/var/lib/postgresql" ("postgres","postgres") [] sql

postgrestconfig :: PGDatabase -> String
postgrestconfig (PGDatabase dbname (PGUser user pass)) = unlines [
    "db-uri = \"postgres://" <> Text.unpack user <> ":" <> Text.unpack pass <> "@localhost:5432/" <> Text.unpack dbname <> "\""
  , "db-schema = \"public\""
  , "db-anon-role = \"" <> Text.unpack user <> "\""
  , "db-pool = 10"
  , "server-host = \"*4\""
  , "server-port = 3000"
  ]
