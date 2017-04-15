module Devops.Utils.Build(BuildArgs(..), asBinaryName, asStackArg
                         , stackInDocker, ImageName(..)) where

import           Data.Functor
import           Data.String
import           Devops.Utils.Docker
import           Devops.Utils.IO
import           System.Directory
import           System.IO
import           System.Process


-- | Arguments passed to `stack` when building desired target
data BuildArgs = SimpleTarget String
               -- ^A simply named target for stack, assumes component is unique in all current packages. See <Stack https://docs.haskellstack.org/en/stable/build_command/#target-syntax>.
               | FullTarget String String
               -- ^A fully named target for stack to build. Assumes component is an executable type.
               | GHCOption String
               -- ^Pass arguments to GHC
               | MoreArgs BuildArgs BuildArgs
               -- ^Compose arguments
               | NoArgs
               -- ^Neutral element for `BuildArgs`
  deriving (Eq, Show, Read)

instance Monoid BuildArgs where
  mempty  = NoArgs
  mappend = MoreArgs

instance IsString BuildArgs where
  fromString = SimpleTarget

asStackArg :: BuildArgs -> [String]
asStackArg NoArgs              = []
asStackArg (SimpleTarget t)    = [":" ++ t]
asStackArg (FullTarget pref t) = [pref ++ ":exe:" ++ t]
asStackArg (GHCOption opt)     = ["--ghc-options", opt]
asStackArg (MoreArgs l r)      = asStackArg l ++ asStackArg r

asBinaryName :: BuildArgs -> String
asBinaryName NoArgs           = ""
asBinaryName (SimpleTarget t) = t
asBinaryName (FullTarget _ t) = t
asBinaryName (GHCOption _)    = ""
asBinaryName (MoreArgs l r)   = asBinaryName l ++ asBinaryName r


-- | Build a Haskell project using some docker image.
--
-- In order to maximize reuse, this process creates in the current directory a file called `.cidfile` which contains
-- the id of the latest container that ran the build. When this file exists, the next run will reuse the volumes of
-- the previous run which means built dependencies will normally be available.
--
-- The built target, which is assumed to be a binary executable, is then extracted from the container and copied
-- locally in a file named after `asBinaryName`.
--
-- TODO: run with current user in the container or reuse stack's docker capabilities
stackInDocker :: ImageName -> FilePath -> BuildArgs -> IO FilePath
stackInDocker img@(ImageName imgName) srcDir buildTarget = do
  absSrcDir <- canonicalizePath srcDir
  buildAlreadyRun <- doesFileExist ".cidfile"
  if buildAlreadyRun
    then do
    cid <- readFile ".cidfile"
    removeFile ".cidfile"
    callProcess "docker" $ ["run", "--cidfile=.cidfile", "-v", absSrcDir ++ ":/build", "--volumes-from=" ++ cid,
                          "-v", "/root/.stack", "-w", "/build" , imgName, "stack", "build","--allow-different-user" ] ++  asStackArg buildTarget
    else callProcess "docker" $ ["run", "--cidfile=.cidfile", "-v", absSrcDir ++ ":/build",
                               "-v", "/root/.stack", "-w", "/build" , imgName, "stack", "build","--allow-different-user" ] ++  asStackArg buildTarget

  exportBinary img (asBinaryName buildTarget)


exportBinary :: ImageName -> String -> IO FilePath
exportBinary (ImageName imgName) targetName = do
  cid <- readFile ".cidfile"
  let reuseVolumes = if not (null cid)
                     then "--volumes-from=" ++ cid
                     else ""
  stackRoot <- filter (/= '\n') <$> readProcess "docker" [ "run", "--rm",reuseVolumes ,  "-w", "/build", imgName, "stack", "path",  "--allow-different-user", "--local-install-root" ] ""
  (_, Just hout, _, phdl) <- createProcess $ (proc "docker" ["run", "--rm", reuseVolumes, "busybox","dd", "if=" ++ stackRoot ++ "/bin/" ++ targetName ]) { std_out = CreatePipe }
  withBinaryFile targetName WriteMode $ \ hDst -> copy hout hDst
  void $ waitForProcess phdl
  return targetName
