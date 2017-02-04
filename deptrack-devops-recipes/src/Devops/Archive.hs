{-# LANGUAGE OverloadedStrings #-}

-- | Actions to manipulate archives.
module Devops.Archive where

import           Data.Monoid            ((<>))
import qualified Data.Text              as Text
import           Devops.Debian.Commands
import           Devops.Storage
import           Devops.Base
import           Devops.Utils

-- | Extract a given archive into a directory, stripping some number of leading components
-- Returns the target directory
untar :: Int -> DevOp FilePresent -> DevOp DirectoryPresent -> DevOp DirectoryPresent
untar strip archiveFile targetDir = devop snd mkOp $ do
    c <- tar
    f <- archiveFile
    d <- targetDir
    pure ((c, f), d)
  where
    mkOp ((c, FilePresent file), DirectoryPresent dir) =
        buildOp ("untar archive: " <> Text.pack file)
        ("extract tar.gz archive  " <> Text.pack file <> " to " <> Text.pack dir)
        noCheck
        (blindRun c ["xvz", "--strip-components=" ++ show strip, "-f", file, "-C", dir] "")
        noAction
        noAction
