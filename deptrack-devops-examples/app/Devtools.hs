{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (void)
import           Data.String.Conversions (convertString)
import           System.Environment (getArgs)

import           Devops.Debian.Base (deb)
import           Devops.Storage (FileContent, fileContent)
import           Devops.Haskell (stackPackage)
import           Devops.Debian.User (mereUser)
import           Devops.Base (DevOp)
import           Devops.Cli (defaultMain)
import           Devops.Optimize (optimizeDebianPackages)

main :: IO ()
main = getArgs >>= defaultMain devtools [optimizeDebianPackages]
  where
    devtools :: DevOp ()
    devtools = void $ do
        packages 
        dotFiles
        ghcid

packages :: DevOp ()
packages = void $ do
    deb "git-core"
    deb "vim"
    deb "tmux"
    deb "graphviz"

dotFiles :: DevOp ()
dotFiles = void $ do
    fileContent "/home/user/.vimrc" dotVimrc
    fileContent "/home/user/.gitconfig" dotGitconfig
    fileContent "/home/user/.bash_profile" dotBashProfile

dotVimrc, dotGitconfig, dotBashProfile :: DevOp FileContent
dotVimrc = pure $ convertString $ unlines $ [
    "syntax on"
  , "filetype plugin indent on"
  , "nnoremap <F1> <ESC><ESC>:tabnew<CR>"
  , "nnoremap <F2> <ESC><ESC>:tabprev<CR>"
  , "nnoremap <F3> <ESC><ESC>:tabnext<CR>"
  , "nnoremap <F4> <ESC><ESC>:tabclose<CR>"
  , "set hlsearch"
  ]
dotGitconfig = pure $ convertString $ unlines $ [
    "[user]"
  , "\temail = lucas@dicioccio.fr"
  , "\tname = Lucas DiCioccio"
  ]
dotBashProfile = pure $ convertString $ unlines $ [
    "# Adds stack-binaries to bash_profile."
  , "export PATH=\"${PATH}:${HOME}/.local/bin\""
  ]


ghcid :: DevOp ()
ghcid = void $ do
    stackPackage "ghcid" (mereUser "user")
