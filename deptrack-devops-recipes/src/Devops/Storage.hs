{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Devops.Storage (
    FilePresent (..)
  , FileContent
  , fileCopy , turnupfileBackup , turndownfileBackup , generatedFile , fileContent
  , fileLink
  , RepositoryFile (..) , localRepositoryFile
  , preExistingFile
  , DirectoryPresent (..) , directory , subdirectory
  , ioFile
  , tarDirectory
  --
  , fileExist
  , blindRemoveLink
  ) where

import           Control.Exception     (catch, IOException)
import qualified Data.ByteString       as ByteString
import           Data.Monoid           ((<>))
import qualified Data.Text             as Text
import           Data.Typeable         (Typeable)
import           System.Directory      (removeDirectory, doesDirectoryExist, createDirectoryIfMissing)
import           System.FilePath.Posix ((</>), takeDirectory)
import           System.Posix.Files    (createSymbolicLink, fileExist)

import           DepTrack
import           Devops.Binary
import           Devops.Base
import           Devops.Utils
import           Devops.Storage.Base
import qualified Devops.Debian.Commands as Cmd

blindRemoveDir :: FilePath -> IO ()
blindRemoveDir path = removeDirectory path
  `catch` (\ (e :: IOException) -> print $ "exception while removing directory " ++ path ++ ": " ++ show e)

-- | states that a file pre-exists
preExistingFile :: FilePath -> DevOp FilePresent
preExistingFile path =
  let mkop = noop ("file-exists: " <> Text.pack path)
                  ("ensures that " <> Text.pack path <> " is present.")
  in declare mkop (pure $ FilePresent path)

-- | states that a given file must be present in a local repository
localRepositoryFile :: FilePath -> DevOp RepositoryFile
localRepositoryFile path =
  let mkop = noop ("file-exists: " <> Text.pack path)
                ("ensures that " <> Text.pack path <> " is present.")
  in declare mkop (pure $ LocalRepositoryFile path)

-- | generates a directory recursively
directory :: FilePath -> DevOp DirectoryPresent
directory path =
  let mkOp _ = buildOp ("directory-exists: " <> Text.pack path)
                       ("non-recursively builds " <> Text.pack path)
                       (fromBool <$> doesDirectoryExist path)
                       (createDirectoryIfMissing False path)
                       (blindRemoveDir path)
                       noAction
  in devop id mkOp (pure $ DirectoryPresent path)

subdirectory :: DevOp DirectoryPresent -> FilePath -> DevOp DirectoryPresent
subdirectory parent path = devop id mkOp $ do
    (DirectoryPresent p) <- parent
    return (DirectoryPresent $ p </> path)
  where
    mkOp (DirectoryPresent fullpath) =
             buildOp ("directory-exists: " <> Text.pack fullpath)
                     ("non-recursively builds " <> Text.pack fullpath)
                     (fromBool <$> doesDirectoryExist fullpath)
                     (createDirectoryIfMissing False fullpath)
                     (blindRemoveDir fullpath)
                     noAction

-- | copies a file from an origin repository
fileCopy :: DevOp FilePath -> DevOp RepositoryFile -> DevOp (RepositoryFile, FilePresent)
fileCopy mkPath mkSrc = devop fst mkop $ do
    cp <- Cmd.cp
    sync <- Cmd.sync
    src@(LocalRepositoryFile srcPath) <- mkSrc
    path <- mkPath
    return ((src, FilePresent path), (cp, sync, srcPath, path))
  where
    fs sync = blindRun sync [] ""
    mkop (_, (cp, sync, srcPath, path)) =
              buildOp ("file-copied: " <> Text.pack path)
                      ("copies " <> Text.pack path <> " from " <> Text.pack srcPath)
                      (checkFilePresent path)
                      (fs sync >> blindRun cp [srcPath, path] "" >> fs sync)
                      (blindRemoveLink path)
                      noAction

tarDirectory :: FilePath -> DevOp DirectoryPresent -> DevOp RepositoryFile
tarDirectory rpath src = devop fst mkOp $ do
    dir <- src
    tar <- Cmd.tar
    sync <- Cmd.sync
    return $ (LocalRepositoryFile rpath, (tar, dir, sync))
  where
    fs sync = blindRun sync [] ""
    mkOp (_, (tar,(DirectoryPresent dir),sync)) =
            buildOp ("tar-directory-backup: " <> Text.pack rpath)
                    ("backup " <> Text.pack dir <> " at " <> Text.pack rpath)
                    (checkFilePresent rpath)
                    (fs sync >> blindRun tar ["-f", rpath, "-C", dir, "-c", "."] "" >> fs sync)
                    (blindRemoveLink rpath)
                    noAction

-- | backup a file at turndown
turndownfileBackup :: FilePath -> DevOp FilePresent -> DevOp (RepositoryFile, FilePresent)
turndownfileBackup rpath src = devop fst mkOp $ do
    file <- src
    cp <- Cmd.cp
    sync <- Cmd.sync
    return $ ((LocalRepositoryFile rpath, file), (cp,sync))
  where 
    fs sync = blindRun sync [] ""
    mkOp ((_, FilePresent srcPath), (cp,sync)) =
            buildOp ("file-backup: " <> Text.pack rpath)
                    ("backup " <> Text.pack srcPath <> " at " <> Text.pack rpath)
                    noCheck
                    noAction
                    (fs sync >> blindRun cp [srcPath, rpath] "" >> fs sync)
                    noAction

-- | Backup a file at turnup.
turnupfileBackup :: FilePath -> DevOp FilePresent -> DevOp (RepositoryFile, FilePresent)
turnupfileBackup rpath src = devop fst mkOp $ do
    file <- src
    cp <- Cmd.cp
    sync <- Cmd.sync
    return $ ((LocalRepositoryFile rpath, file), (cp,sync))
  where
    fs sync = blindRun sync [] ""
    mkOp ((_, FilePresent srcPath), (cp,sync)) =
            buildOp ("file-backup: " <> Text.pack rpath)
                    ("backup " <> Text.pack srcPath <> " at " <> Text.pack rpath)
                    noCheck
                    (fs sync >> blindRun cp [srcPath, rpath] "" >> fs sync)
                    noAction
                    noAction

-- | Links a file from an originally present file.
fileLink :: FilePath -> DevOp FilePresent -> DevOp (FilePresent, FileLinked)
fileLink path mkSrc = devop id mkop $ do
    x@(FilePresent srcPath) <- mkSrc
    return (x, FileLinked srcPath path Nothing)
  where
    mkop ((FilePresent srcPath),_) =
              buildOp ("file-linked: " <> Text.pack path)
                      ("links " <> Text.pack path <> " at " <> Text.pack srcPath)
                      (checkFilePresent path)
                      (createSymbolicLink srcPath path)
                      (blindRemoveLink path)
                      noAction

-- | Generates a file from an external command.
--
-- At the moment the external command should only produce a single file,
-- otherwise there is a risk of not cleaning-up correctly.
generatedFile :: Typeable a
              => FilePath
              -> DevOp (Binary a) -- ^ binary to build
              -> DevOp [String] -- ^ arguments to binary
              -> DevOp (Binary a, [String], FilePresent)
generatedFile path mkBinary mkArgs = devop fst mkOp $ do
    _ <- directory (takeDirectory path)
    args <- mkArgs
    cmd <- mkBinary
    sync <- Cmd.sync
    return $ ((cmd, args, FilePresent path),sync)
  where 
    fs sync = blindRun sync [] ""
    mkOp ((cmd, args, _),sync) =
             buildOp ("file-generated: " <> Text.pack path)
                     ("invokes `" <> Text.pack (binaryPath cmd) <> "` with" <> Text.pack (show args))
                     (checkFilePresent path)
                     (blindRun cmd args "" >> fs sync)
                     (blindRemoveLink path) -- can we ensure there will be no other side-effects?
                     noAction

-- | Checks that a file is present.
checkFilePresent :: FilePath -> IO CheckResult
checkFilePresent = fmap fromBool . fileExist

-- | Generates a file with an IO-action.
ioFile :: FilePath
       -> DevOp (IO FileContent)
       -> DevOp FilePresent
ioFile path mkIO = devop fst mkOp $ do
    action <- mkIO
    sync <- Cmd.sync
    return (FilePresent path, (action,sync))
  where
    fs sync = blindRun sync [] ""
    mkOp (_,(io,sync)) =
                  buildOp ("file-generated: " <> Text.pack path)
                          ("generates a file with an IO action")
                          (checkFilePresent path)
                          (io >>= ByteString.writeFile path >> fs sync)
                          (blindRemoveLink path)
                          noAction
