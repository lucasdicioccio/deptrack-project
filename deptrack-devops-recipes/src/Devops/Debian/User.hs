{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO extract generic part to Devops.Unix ?
module Devops.Debian.User (
    Group (..) , group
  , User (..) , user , mereUser
  , preExistingGroup , preExistingUser
  , noExtraGroup
  , Ownership , filePermissions
  , directoryPermissions
  , userDirectory, homeDirPath
  ) where

import           Control.Applicative     (liftA2)
import           Control.Exception
import           Data.Monoid             ((<>))
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           System.FilePath.Posix   ((</>))
import           System.Posix.Files      (setOwnerAndGroup)
import           System.Posix.User       (getGroupEntryForName,
                                          getUserEntryForName, groupID, userID)

import           Devops.Debian.Commands
import           Devops.Storage
import           Devops.Base
import           Devops.Utils

type UserName = Text
type GroupName = Text
newtype User = User { userName :: UserName }
newtype Group = Group { groupName :: GroupName }

group :: GroupName -> DevOp env Group
group n = devop snd mkop $ do
    gadd <- groupadd
    gdel <- groupdel
    return $ ((gadd, gdel), Group n)
  where
    mkop ((gadd,gdel), _) =
             buildOp ("debian-group: " <> convertString n)
                     ("ensures that " <> convertString n <> " is a group.")
                     (raisesNot $ getGroupEntryForName (Text.unpack n))
                     (blindRun gadd [convertString n] "")
                     (blindRun gdel [convertString n] "")
                     noAction

preExistingGroup :: GroupName -> DevOp env Group
preExistingGroup n = pure $ Group n

preExistingUser :: UserName -> DevOp env User
preExistingUser n = pure $ User n

noExtraGroup :: DevOp env [Group]
noExtraGroup = pure []

raisesNot :: IO a -> IO CheckResult
raisesNot act = (act >> return Success) `catch` (\(e::IOException) -> return $ Failure (show e))

user :: UserName -> DevOp env Group -> DevOp env [Group] -> DevOp env User
user n mkBaseGroup mkGroups = devop snd mkop $ do
  uadd <- useradd
  udel <- userdel
  basegrp <- groupName <$> mkBaseGroup
  grps <- map groupName <$> mkGroups
  return $ ((uadd, udel, basegrp, grps), User n)
  where
    mkop ((uadd, udel, basegrp, grps), _) =
             buildOp ("debian-user: " <> convertString n)
                     ("ensures that " <> convertString n <> " is a user.")
                     (raisesNot $ getUserEntryForName (Text.unpack n))
                     (blindRun uadd (uaddParams basegrp grps n) "")
                     (blindRun udel [convertString n] "")
                     noAction

uaddParams :: GroupName -> [GroupName] -> UserName -> [String]
uaddParams grp [] u = ["-M", "-c", "user-added-via-devops", "-g", convertString grp, convertString u]
uaddParams grp grps n = ["-M", "-G", convertString $ extraGroups grps,"-c", "user-added-via-devops", "-g", convertString grp, convertString n]
  where extraGroups gs = Text.intercalate "," gs

type Ownership = (User,Group)

filePermissions :: DevOp env Ownership -> DevOp env FilePresent -> DevOp env FilePresent
filePermissions mkOwner mkFile = fmap fst $ track (uncurry mkFilePathPermissionOp) $ do
  (User usr, Group grp) <- mkOwner
  fp@(FilePresent path) <- mkFile
  return (fp, (path,usr,grp))

directoryPermissions :: DevOp env Ownership -> DevOp env DirectoryPresent -> DevOp env DirectoryPresent
directoryPermissions mkOwner mkDir = fmap fst $ track (uncurry mkFilePathPermissionOp) $ do
  (User usr, Group grp) <- mkOwner
  dir@(DirectoryPresent path) <- mkDir
  return (dir, (path,usr,grp))

mkFilePathPermissionOp :: a -> (FilePath, UserName, GroupName) -> PreOp
mkFilePathPermissionOp  _ (path,usr,grp) =
  buildPreOp ("change-permissions: " <> Text.pack path)
     ("changes permissions on " <> Text.pack path <> " to " <> usr <> ":" <> grp)
     (noCheck)
     (chownFile usr grp path)
     (noAction)
     (noAction)

chownFile :: UserName -> GroupName -> FilePath -> IO ()
chownFile username groupname path = do
            usr <- getUserEntryForName (convertString username)
            grp <- getGroupEntryForName (convertString groupname)
            setOwnerAndGroup path (userID usr) (groupID grp)

mereUser :: Name -> DevOp env User
mereUser name = user name (group name) noExtraGroup

userDirectory :: FilePath -> DevOp env User -> DevOp env DirectoryPresent
userDirectory subpath mkUser = do
  -- just extract the name of the user, fine to not track here because we
  -- re-inject the dep inside directoryPermissions
  (User name) <- mkUser
  let user_group = liftA2 (,) mkUser (group name)
  let homedir = directoryPermissions (user_group) (directory (homeDirPath name))
  directoryPermissions (user_group) (subdirectory homedir subpath)

homeDirPath :: Name -> FilePath
homeDirPath "root" = "/root"
homeDirPath name   = "/home" </> Text.unpack name
