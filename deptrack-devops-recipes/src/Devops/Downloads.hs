{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Devops.Downloads where

import           Data.Monoid            ((<>))
import qualified Data.Text              as Text
import           DepTrack               (track)

import           Devops.Debian.Commands
import           Devops.Storage
import           Devops.Base
import           Devops.Utils

type URLString = String

data DownloadedFile = DownloadedFile { downloadedFrom :: !URLString
                                     , downloadedTo   :: !FilePath
                                     }

download :: URLString -> FilePath -> DevOp DownloadedFile
download url path = devop fst mkop $ do
    b <- wget
    return (DownloadedFile url path, b)
  where
    mkop (!_,bin) = buildOp ("file-downloaded: " <> Text.pack path)
                            ("uses wget to download a file")
                            (fromBool <$> fileExist path)
                            (blindRun bin ["-O", path, url] "")
                            (blindRemoveLink path)
                            noAction
