{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Devops.Bootstrap.Build where

import           Data.Functor       (void)
import           Data.Monoid
import           Data.Proxy
import           Data.Text          (pack)
import           Data.Typeable
import           Devops.Base
import           Devops.Binary
import           Devops.OS
import           Devops.Storage
import           Devops.Utils.Build
import           System.Directory   (doesFileExist)


-- | A specific build of (self) `DepTrack` application for a given OS/arch target
data Build (o :: OS) = Build { remotableExec   :: BuildArgs
                             , sourceDirectory :: FilePath
                             }
                    deriving (Eq, Show, Typeable)

class HasDockerImage b where
  dockerImage :: b -> ImageName

instance HasDockerImage (Build 'Ubuntu14_04) where
  dockerImage _ = ImageName "haskell:8.0.2"

instance HasDockerImage (Build 'Ubuntu16_04) where
  dockerImage _ = ImageName "haskell:8.0.2"

build :: (Typeable o, HasDockerImage (Build o)) => Proxy o -> FilePath -> BuildArgs -> DevOp (Build o)
build _ sourceDir buildTarget = devop snd mkOp $ do
  d <- binary :: DevOp (Binary "dockerX")  -- we need docker installed...
  return (d,Build buildTarget sourceDir)
    where
      mkOp (_, b@Build{..}) = buildOp
        ("OS-dependent binary: " <> pack (show buildTarget))
        ("creates OS-dependent remotable binary: " <> pack (show b))
        (fromBool <$> doesFileExist (asBinaryName buildTarget))
        (buildRemotable b)
        (blindRemoveLink $ asBinaryName buildTarget)
        noAction

buildOutput :: (Typeable o) => DevOp (Build o)  -> DevOp FilePresent
buildOutput built = devop id mkOp $
  built >>= pure . FilePresent . asBinaryName . remotableExec
    where
      mkOp (FilePresent fp) = buildOp
        ("build-output: " <> pack fp)
        "file output from a build"
        (fromBool <$> doesFileExist fp)
        noAction
        noAction
        noAction

buildRemotable :: (HasDockerImage (Build o)) => Build o -> IO ()
buildRemotable b@Build{..} = void $ stackInDocker (dockerImage b) sourceDirectory remotableExec
