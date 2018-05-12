{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Monad             (guard, void)
import           Control.Monad.Reader      (ReaderT, runReaderT, asks, lift, local)
import           Control.Applicative       ((<|>))
import           Devops.Binary             (binary)
import           Devops.Base               (DevOp, DevOpT)
import           Devops.Cli                (App (..), Method (..), SelfPath,
                                            appMain, appMethod, methodArg)
import           Devops.Debian.Base        (deb)
import           Devops.Debian.Commands    (git)

import           Devops.Binary

type OS = String

onlyFor :: OS -> DevOp OS a -> DevOp OS a
onlyFor x f = do
  asks (==x) >>= guard
  f

localGit :: DevOp OS (Binary "git")
localGit = binary

debianGit :: DevOp OS (Binary "git")
debianGit = onlyFor "debian" $ git

portableGit :: DevOp OS (Binary "git")
portableGit =
      debianGit
  <|> localGit

main :: IO ()
main = do
    let stages _ _ _ = void $ portableGit
    let app = App (\(x:y:[]) -> (y, appMethod x))
                  undefined
                  stages
                  []
                  (\y -> return y)
    appMain app

