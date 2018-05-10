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
type EnvDevOp = ReaderT OS (DevOp :: * -> *)

onlyFor :: OS -> EnvDevOp a -> EnvDevOp a
onlyFor x f = do
  asks (==x) >>= guard
  f

localGit :: EnvDevOp (Binary "git")
localGit = lift $ binary

debianGit :: EnvDevOp (Binary "git")
debianGit = onlyFor "debian" $ do
  lift $ git

portableGit :: EnvDevOp (Binary "git")
portableGit =
      debianGit
  <|> localGit

main :: IO ()
main = do
    let stages n _ _ = runReaderT n portableGit
    let app = App (\(x:y:[]) -> (y, appMethod x))
                  undefined
                  (\n _ _ -> runReaderT (local (const "debian") $ void portableGit) n)
                  []
    appMain app

