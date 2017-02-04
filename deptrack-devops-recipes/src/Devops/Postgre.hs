{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Devops.Postgre where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Monoid ((<>))

import           Devops.Base (DevOp, buildOp, devop, noAction, noCheck)
import           Devops.Binary (Binary, HasBinary, binary)
import           Devops.Debian.Base (DebianPackage, debianPackage, deb, installedWith)
import           Devops.Debian (sudoRunAsInDir)

data PGUser = PGUser {
    userName :: !Text
  , userPass :: !Text
  } deriving (Eq, Ord, Show)

data PGDatabase = PGDatabase {
    dbName  :: !Text
  , dbOwner :: !PGUser
  } deriving (Eq, Ord, Show)

instance HasBinary (DebianPackage "postgresql") "createdb" where
instance HasBinary (DebianPackage "postgresql") "dropdb" where
instance HasBinary (DebianPackage "postgresql") "createuser" where
instance HasBinary (DebianPackage "postgresql") "dropuser" where
instance HasBinary (DebianPackage "postgresql") "psql" where

postgresql :: DevOp (DebianPackage "postgresql")
postgresql = debianPackage

libpqDev :: DevOp (DebianPackage "libpq-dev")
libpqDev = deb "libpq-dev"

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

-- | A PostGre database owned by a given PostGre user.
pgDatabase :: Text
           -- ^ database name
           -> DevOp PGUser
           -- ^ database owner
           -> DevOp PGDatabase
pgDatabase n mkU = devop snd mkOp $ do
    user@(PGUser u _) <- mkU
    c <- createdb
    d <- dropdb
    return $ ((u,c,d), PGDatabase n user)
  where
    mkOp ((u,createD,dropD),_) =
        buildOp ("postgres-database: " <> n)
                ("creates a PostGre DB owned by user" <> u)
                noCheck
                (sudoRunAsInDir createD "/var/lib/postgresql" ("postgres","postgres") [ "-e", "-O", Text.unpack u, Text.unpack n] "")
                (sudoRunAsInDir dropD "/var/lib/postgresql" ("postgres","postgres") ["-e", Text.unpack n] "")
                noAction
    
-- | A PostGre database user.
pgUser :: Text
       -- ^ username
       -> Text
       -- ^ user-password
       -> DevOp PGUser
pgUser username pass = devop snd mkOp $ do
    c <- createuser
    s <- psql
    d <- dropuser
    return ((c,d,s), PGUser username pass)
  where
    mkOp ((createU,dropU,psqlShell),_) =
        buildOp ("postgres-user: " <> username)
                ("creates a PostGre user")
                noCheck
                (sudoRunAsInDir createU "/var/lib/postgresql" ("postgres","postgres") ["-e", Text.unpack username] ""
                 >> superPsqlShell psqlShell ("ALTER USER " <> Text.unpack username <> " WITH PASSWORD '" <> Text.unpack pass <> "';"))
                (sudoRunAsInDir dropU "/var/lib/postgresql" ("postgres","postgres") ["-e", Text.unpack username] "")
                noAction

-- | Utility to run raw commands in psql as the superuser postgres.
superPsqlShell :: Binary "psql" -> String -> IO ()
superPsqlShell shell sql =
    sudoRunAsInDir shell "/var/lib/postgresql" ("postgres","postgres") [] sql
