{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

-- | A module to run services as simple daemons.
--
-- Daemons of different types will expect different configurations. Hence, this
-- module use an open type-family to have the DaemonConfig depends on the type
-- of Daemon implemented in a service.
module Devops.Service (
    Daemon (..) , DaemonConfig
  , CommandArgs
  , daemon
  , reloadableDaemon
  , sighupPidFile , sighupDaemon
  ) where

import           Control.Concurrent      (threadDelay)
import           Control.Exception       (IOException, catch)
import           Control.Monad           (void, when)
import           Data.List               (intercalate, isSubsequenceOf)
import           Data.Monoid             ((<>))
import           Data.String.Conversions (convertString)
import qualified Data.Text               as Text
import           Data.Typeable           (Typeable)
import           GHC.TypeLits            (KnownSymbol)
import           Prelude                 hiding (readFile)
import           System.FilePath.Posix   ((</>))
import           System.IO.Strict        (readFile)
import           System.Posix.Daemonize  (daemonize)
import           System.Posix.Files      (removeLink)
import           System.Posix.Process    (executeFile, forkProcess,
                                          getProcessID)
import           System.Posix.Signals    (sigHUP, sigKILL, sigTERM,
                                          signalProcess)
import           System.Posix.Types      (ProcessID)
import           System.Posix.User       (GroupEntry (..), UserEntry (..),
                                          getGroupEntryForName,
                                          getUserEntryForName, setGroupID,
                                          setUserID)
import           Text.Printf             (printf)

import           Devops.Binary
import           Devops.Debian.User
import           Devops.Storage
import           Devops.Base

data Daemon a = Daemon !Name !FilePath !(Maybe (User,Group)) !(DaemonConfig a)
type CommandArgs = [String]

type family DaemonConfig a :: *

-- | A simple daemon implementation.
--
-- It starts a daemonized process that writes its PID to
-- /var/run/devops/<daemon-name> and then execs a command. The daemon is not
-- supervised therefore, daemon is well-suited for battle-tested
-- servers.
-- Process' life is determined by checking that /proc/<pid>/cmdline
-- matches the expected values.
-- On turndown, the process is sent a SIGTERM and, after three seconds, a
-- SIGKILL.
daemon :: (Typeable a, KnownSymbol path) =>
     Name
  -> Maybe (DevOp (User, Group))
  -> DevOp (Binary path)
  -> (DaemonConfig a -> CommandArgs)
  -> DevOp (DaemonConfig a)
  -> DevOp (Daemon a)
daemon name mkUserGroup mkBin toCmdArgs config = devop snd mkOp $ do
    DirectoryPresent dirPath <- directory "/var/run/devops"
    let pidFile = dirPath </> Text.unpack name
    user_group <- sequence mkUserGroup
    let d = (Daemon name pidFile user_group <$> config)
    (,) <$> mkBin <*> d
  where
    mkOp (Binary cmd, d@(Daemon _ _ _ cfg)) =
        let args = toCmdArgs cfg in
        buildOp ("daemon: " <> name)
                ("runs daemon using `" <> Text.intercalate " " (fmap Text.pack (cmd:args)) <> "`")
                (checkDaemonRunning d cmd args)
                (runDaemon d cmd args)
                (killDaemon d)
                noAction

reloadableDaemon :: (Typeable a, KnownSymbol path)
  => Name
  -> Maybe (DevOp (User, Group))
  -> DevOp (Binary path)
  -> (DaemonConfig a -> CommandArgs)
  -> (Daemon a -> OpAction)
  -> DevOp (DaemonConfig a)
  -> DevOp (Daemon a)
reloadableDaemon name mkUserGroup mkBin toCmdArgs reloadAction config =
    devop snd mkOp $ do
        DirectoryPresent dirPath <- directory "/var/run/devops"
        let pidFile = dirPath </> Text.unpack name
        user_group <- sequence mkUserGroup
        let d = (Daemon name pidFile user_group <$> config)
        (,) <$> mkBin <*> d
  where
    mkOp (Binary cmd, d@(Daemon _ _ _ cfg)) =
        let args = toCmdArgs cfg in
        buildOp ("daemon: " <> name)
                ("runs daemon using `" <> Text.pack (show (cmd, args)) <> "`")
                (checkDaemonRunning d cmd args)
                (runDaemon d cmd args)
                (killDaemon d)
                (reloadAction d)

procCmdLinePath :: String -> FilePath
procCmdLinePath pid = "/proc/"++pid++"/cmdline"

procCmdLineExpectedValue :: String -> [String] -> String
procCmdLineExpectedValue cmd args = intercalate "\NUL" $ (cmd:args) ++ [""]

procCmdLineMatches :: String -> [String] -> String -> Bool
procCmdLineMatches cmd args got =
  let exactMatch = (procCmdLineExpectedValue cmd args == got)
      hasBin = (cmd `isSubsequenceOf` got)
      hasArgs = and (fmap (\arg -> arg `isSubsequenceOf` got) args)
  in exactMatch || (hasBin && hasArgs)

checkDaemonRunning :: Daemon a -> String -> [String] -> OpCheck
checkDaemonRunning (Daemon _ pidFile _ _) cmd args =
    go `catch` handleIOException
  where
    handleIOException e = swallowIOException e >> return (Failure $ show e)
    go = do
        pid <- readFile pidFile
        got <- readFile (procCmdLinePath pid)
        let expected = procCmdLineExpectedValue cmd args
        let !acceptable = procCmdLineMatches cmd args got
        if acceptable
        then return Success
        else return $ Failure (printf "got `%s` expecting something like `%s`" got expected)

sighupDaemon :: Daemon a -> IO ()
sighupDaemon (Daemon _ pidFile _ _) = sighupPidFile pidFile

runDaemon :: Daemon a -> String -> [String] -> OpAction
runDaemon (Daemon _ pidFile user_group _) cmd args =
    void $ forkProcess $ daemonize $ do
        pid <- getProcessID
        writeFile pidFile (show pid)
        maybe (return ()) (uncurry changeUserAndGroup) user_group
        executeFile cmd False args Nothing
  where
    changeUserAndGroup :: User -> Group -> IO ()
    changeUserAndGroup (User u) (Group g) = do
        getGroupEntryForName (convertString g) >>= setGroupID . groupID
        getUserEntryForName (convertString u) >>=  setUserID . userID

readPidFile :: FilePath -> IO (Maybe ProcessID)
readPidFile pidFile =
  (Just . read <$> readFile pidFile)
  `catch` (\e -> swallowIOException e >> return Nothing)

removePidFile :: FilePath -> IO ()
removePidFile path = do
  exist <- fileExist path
  when exist $ removeLink path

sighupPidFile :: FilePath -> IO ()
sighupPidFile pidFile = do
  pid <- readPidFile pidFile
  maybe (return ()) (signalProcess sigHUP) pid

killDaemon :: Daemon a -> OpAction
killDaemon (Daemon _ pidFile _ _) = do
    pid <- readPidFile pidFile
    maybe (return ()) killPidAndRemovePidFile pid
  where
    killPidAndRemovePidFile p = do
        let gracePeriod = threadDelay 3000000
        let sendTERM = signalProcess sigTERM p
        let sendKILL = signalProcess sigKILL p
        (sendTERM >> gracePeriod >> sendKILL) `catch` swallowIOException
        removePidFile pidFile

swallowIOException :: IOException -> IO ()
swallowIOException e = putStrLn ("caught " ++ show e)
