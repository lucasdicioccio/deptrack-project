{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (void)
import           Control.Applicative ((<|>))
import           Data.String.Conversions (convertString)
import           System.Environment (getArgs)

import           Devops.Debian.Base (deb)
import           Devops.MacOS.Base (brew)
import           Devops.Storage (FileContent, fileContent)
import           Devops.Haskell (stackPackage)
import           Devops.Debian.User (mereUser)
import           Devops.Base (DevOp)
import           Devops.Constraints (HasOS(..), onOS)
import           Devops.Cli (simpleMain)
import           Devops.Optimize (optimizeDebianPackages)

data Env = Env String
instance HasOS Env where
  os (Env n) = n

main :: IO ()
main = do
    (e:args) <- getArgs
    simpleMain devtools [optimizeDebianPackages] args (Env e)
  where
    devtools :: HasOS env => DevOp env ()
    devtools = void $ do
        packages 
        onOS "debian" dotFiles <|> return ()
        onOS "debian" ghcid <|> return ()

packages :: HasOS env => DevOp env ()
packages = debianLike <|> macOSLike
  where
    debianLike = onOS "debian" $ void $ do
        deb "git-core"
        deb "vim"
        deb "tmux"
        deb "graphviz"
    macOSLike = onOS "mac-os" $ void $ do
        brew "tree"
        brew "tmux"
        brew "graphviz"

dotFiles :: DevOp env ()
dotFiles = void $ do
    fileContent "/home/user/.vimrc" dotVimrc
    fileContent "/home/user/.gitconfig" dotGitconfig
    fileContent "/home/user/.bash_profile" dotBashProfile

dotVimrc, dotGitconfig, dotBashProfile :: DevOp env FileContent
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

ghcid :: DevOp env ()
ghcid = void $ do
    stackPackage "ghcid" (mereUser "user")
