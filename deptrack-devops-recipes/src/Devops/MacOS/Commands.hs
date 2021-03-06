{-# LANGUAGE DataKinds #-}
module Devops.MacOS.Commands where

import Devops.Base
import Devops.Binary (Binary, binary)
import Devops.MacOS.Base (installedWith)
import qualified Devops.MacOS.Packages as Pkg

docker :: DevOp env (Binary "docker")
docker = binary `installedWith` Pkg.docker

dockerMachine :: DevOp env (Binary "docker-machine")
dockerMachine = binary `installedWith` Pkg.dockerMachine

vboxManage :: DevOp env (Binary "VBoxManage")
vboxManage = binary `installedWith` Pkg.virtualBox

git :: DevOp env (Binary "git")
git = binary `installedWith` Pkg.git

dot :: DevOp env (Binary "dot")
dot = binary `installedWith` Pkg.graphviz

twopi :: DevOp env (Binary "twopi")
twopi = binary `installedWith` Pkg.graphviz

neato :: DevOp env (Binary "neato")
neato = binary `installedWith` Pkg.graphviz
