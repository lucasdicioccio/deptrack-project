{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (void)
import           Data.String.Conversions (convertString)
import           System.Environment (getArgs)
import           Data.Tree                   (Forest)
import           DepTrack                    (GraphData, buildGraph,
                                              evalDepForest1)

import           Devops.Debian.Base (deb)
import           Devops.Storage (fileContent)
import           Devops.Haskell (stackPackage)
import           Devops.Debian.User (mereUser)
import           Devops.Base
import           Devops.Actions
import           Devops.Cli
import           Devops.Optimize

main :: IO ()
main = do
    let forest = getDependenciesOnly devtools
    applyMethod [optimizeDebianPackages] forest TurnUp

devtools :: DevOp ()
devtools = void $ do
    traverse deb [ "git-core" , "vim" , "tmux" , "graphviz" ]
    -- some configs, assuming the system user is 'user'
    fileContent "/home/user/.vimrc" dotVimrc
    fileContent "/home/user/.gitconfig" dotGitconfig
    fileContent "/home/user/.bash_profile" dotBashProfile
    -- some stack packages
    stackPackage "ghcid" (mereUser "user")

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

