{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Devops.Storage.Base (
    FilePresent (..)
  , FileContent , fileContent
  , RepositoryFile (..)
  , DirectoryPresent (..)
  , FileLinked (..)
  , blindRemoveLink
  ) where

import           Control.Exception     (catch, IOException)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as ByteString
import           Data.Monoid           ((<>))
import qualified Data.Text             as Text
import           System.Directory      (createDirectoryIfMissing)
import           System.FilePath.Posix (takeDirectory)
import           System.Posix.Files    (removeLink)

import           Devops.Base
import           Devops.Utils

type FileContent = ByteString

data FilePresent = FilePresent { getFilePresentPath :: !FilePath }

data DirectoryPresent = DirectoryPresent !FilePath
  deriving Show

type SHA1 = String -- TODO: improve
data FileLinked = FileLinked !FilePath !FilePath !(Maybe SHA1)

data RepositoryFile = LocalRepositoryFile !FilePath


-- | Generates the content of a File
fileContent :: FilePath -> DevOp FileContent -> DevOp (FileContent, FilePresent)
fileContent path mkContent = devop id mkop $ do 
    !content <- mkContent
    return (content, FilePresent path)
  where
    correctContent expectedContent = do
            gotBlob <- ByteString.readFile path
            return $ fromBool (expectedContent == gotBlob)
    mkop (!content, _) =
            buildOp ("file-generated: " <> Text.pack path)
                    ("applies a recipe to build the content of the file")
                    (correctContent content
                     `catch` (\e -> putStrLn ("caught:" ++ show (e::IOException))
                                >> (return $ Failure $ show e)))
                    (createDirectoryIfMissing True (takeDirectory path)
                     >> ByteString.writeFile path content)
                    (blindRemoveLink path)
                    noAction

blindRemoveLink :: FilePath -> IO ()
blindRemoveLink path = removeLink path
  `catch` (\ (e :: IOException) -> print $ "exception while removing file " ++ path ++ ": " ++ show e)

