{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Devops.Bootstrap.Parasite where

import           Control.Concurrent
import           Control.Distributed.Closure (Closure, Serializable,
                                              Static (..), cap, cpure,
                                              unclosure)
import           Control.Monad
import qualified Data.Binary                 as Binary
import qualified Data.ByteString.Base64.Lazy as B64
import           Data.Monoid                 ((<>))
import           Data.String.Conversions     (convertString)
import           Data.Text                   (Text, pack, unpack)
import           Data.Typeable
import           DepTrack
import           Devops.Base
import           Devops.Binary
import           Devops.Callback
import           Devops.Cli
import           Devops.Debian.User
import           Devops.Networking           hiding (Remoted (..))
import           Devops.Ref
import           Devops.Storage
import           Devops.Utils
import           System.FilePath

type ParasiteLogin = Text
type Remotable = Resolver Remote

-- | A host that we control
data ParasitedHost = ParasitedHost !FilePath !ParasiteLogin !Remotable

-- | A value on a remote to be resolved.
data Remoted a = Remoted !Remotable !a
  deriving Functor

-- | A file transferred to some referenced host at a given remote path
data FileTransferred = FileTransferred !FilePath !User !Remotable

-- | A SSH server ready on some resolvable remote
data SshReady = SshReady { remoteSsh :: !Remotable }
              deriving (Show)

magicRemoteArgv :: String
magicRemoteArgv = "~~~"

isMagicRemoteArgv :: [ String ] -> Bool
isMagicRemoteArgv args = take 1 args == [magicRemoteArgv]

-- | A parasite that requires some `Remote` resolution to be deployed.
parasite :: DevOp User -> DevOp FilePresent -> DevOp Remotable -> DevOp ParasitedHost
parasite usr selfPath mkRemote = track mkOp $ do
  r <- mkRemote
  (FilePresent selfBinary) <- selfPath
  udir <- inferUserDirectory <$> usr
  let rpath = udir </> takeBaseName selfBinary
  (FileTransferred _ _ _) <- fileTransferred usr selfPath rpath (sshReady mkRemote)
  return (ParasitedHost rpath "user" r)

  where mkOp (ParasitedHost fp _ r) = noop ("parasited-host: " <> pack (show $ resolvedKey r))
          ("parasited host " <> pack (show $ resolvedKey r) <> " with " <> pack fp)

        inferUserDirectory (User "root") = "/root"
        inferUserDirectory (User u)      = "/home" </> unpack u

sshReady :: DevOp Remotable  -> DevOp SshReady
sshReady resolveRemote = devop id mkOp $ SshReady <$> resolveRemote
    where
      mkOp (SshReady r) =
        let rkey = pack (show $ resolvedKey r)
        in  buildOp
            ("SSH server ready: " <> rkey)
            ("ensures SSH server is ready on remote " <> rkey)
            (resolver r >>= \ (Remote ip) -> fromBool <$> trySsh ip 1)
            (resolver r >>= \ (Remote ip) -> tryOrFail ip)
            noAction
            noAction
      tryOrFail ip = trySsh ip 10 >>= \ res -> when (not res) $ fail $ "cannot connect to SSH on remote " <> unpack ip <> " after 10 tries, giving up"

trySsh :: IpNetString -> Int -> IO Bool
trySsh _ 0 = return False
trySsh h n = do
  res <- checkExitCode "ssh" [ "-o","StrictHostKeyChecking=no", "root@" ++ unpack h, "/bin/true" ] ""
  case res of
    Success -> return True
    _       -> threadDelay oneSecond >> trySsh h (n - 1)
      where
        oneSecond = 1 * 1000 * 1000

-- | A file transferred at a remote path.
fileTransferred :: DevOp User -> DevOp FilePresent -> FilePath -> DevOp SshReady -> DevOp FileTransferred
fileTransferred usr mkFp path mkRemote = devop fst mkOp $ do
  u <- usr
  c <- scp
  f <- mkFp
  r <- remoteSsh <$> mkRemote
  return (FileTransferred path u r, (f,c))
  where scp = binary :: DevOp (Binary "scp")
        mkOp (FileTransferred rpath u r, (FilePresent lpath,c)) =
              buildOp ("remote-file: " <> pack rpath <> " @" <> pack (show $ resolvedKey r))
                 ("remote copy file " <> pack lpath <> " to " <> pack (show $ resolvedKey r) <> " as user " <> pack (show $ userName u))
                 noCheck
                 (resolver r >>= \ (Remote ip) -> blindRun c (scpcmd lpath u ip rpath) "")
                 noAction
                 noAction
        scpcmd lpath u ip rpath = [ "-o", "StrictHostKeyChecking no"
                                  , "-o", "UserKnownHostsFile /dev/null"
                                  , "-p"  -- preserves execution flag
                                  , lpath
                                  , unpack (userName u) ++ "@" ++ unpack ip ++ ":" ++ rpath]


-- | Remotely execute a `Closure` on some parasited host
-- adapted from `Devops.Parasite`
-- TODO unify the different ways of talking to a remote host
remoted :: Typeable a
        => (Closure (DevOp a) -> BinaryCall)
        -> DevOp User
        -> Closure (DevOp a)
        -> DevOp ParasitedHost
        -> DevOp (Remoted a)
remoted mkCb usr clo host = devop fst mkOp $ do
    let remoteObj = runDevOp $ unclosure clo
    let (BinaryCall selfPath fArgs) = mkCb clo
    let args = fArgs TurnUp
    u <- usr
    c <- ssh
    (ParasitedHost _ _ r) <- host
    return (Remoted r remoteObj,(selfPath, args, c, u, r))
  where
    ssh = binary :: DevOp (Binary "ssh")
    mkOp (_, (rpath, args, c, u, r)) = buildOp
            ("remote-closure: " <> pack rpath <> " @" <> pack (show $ resolvedKey r))
            ("calls '" <> pack rpath <> "'")
            noCheck
            (resolver r >>= \ (Remote ip) -> blindRun c (sshCmd rpath u ip args) "")
            noAction
            noAction
    sshCmd rpath u ip args = [
        "-o", "StrictHostKeyChecking no"
      , "-o", "UserKnownHostsFile /dev/null"
      , "-l", unpack (userName u), unpack ip
      , remoteExecution rpath args
      ]
    remoteExecution rpath args = unwords $ [
        "chmod", "+x", rpath, ";"
      , rpath
      ] ++ args ++ [ ";" ]

-- | Remotely execute a `Closure` on some parasited host passing it an argument at runtime
--
-- This function exists to handle the case where the remote closure's arguments cannot be known at graph construction
-- time, e.g. when it depends on some dynamic value that is generated when the graph is turned-up. Note the return
-- value which is an opaque remoted value: We cannot construct an actual DevOp node because we cannot run deptrack
-- on the closure's content.
remotedWith :: (Typeable b, Typeable a, Static (Serializable a))
               => DevOp (IO a) -> DevOp User -> Closure (a -> DevOp b) -> DevOp ParasitedHost -> DevOp (Remoted ())
remotedWith mkAction usr clo host =
  devop fst mkOp (do
              u <- usr
              c <- ssh
              act <- mkAction
              (ParasitedHost rpath _ r) <- host
              return (Remoted r (),(rpath, c, clo, u, r, act)))

  where ssh = binary :: DevOp (Binary "ssh")
        mkOp (_, (rpath, c, clos, u, r, act)) = buildOp
              ("remote-deferred-closure: " <> " @" <> pack (show $ resolvedKey r))
              ("calls 'turnup --b64=XXX' at host " <> pack (show $ resolvedKey r))
              noCheck
              (do
                  Remote ip <- resolver r
                  arg <- act
                  let fp = convertString $ B64.encode $ Binary.encode $ clos `cap` cpure closureDict arg
                  blindRun c (sshCmd rpath u ip fp) "")
              noAction
              noAction
        sshCmd rpath u ip b64 = [ "-o", "StrictHostKeyChecking no"
                                , "-o", "UserKnownHostsFile /dev/null"
                                , "-l", unpack (userName u), unpack ip
                                , remoteExecution rpath b64 ]
        remoteExecution rpath b64 = unwords [ "chmod", "+x", rpath, ";", rpath, magicRemoteArgv, unpack b64 , ";"]


-- | Remotely callback some `Continued a` on some parasited host.
--
remoteContinued :: (Typeable a)
               => DevOp User -> Continued a -> DevOp ParasitedHost -> DevOp (Remoted a)
remoteContinued usr cont host =
  devop fst mkOp (do
              u <- usr
              c <- ssh
              let obj = eval cont
                  (BinaryCall _ fArgs) = callback cont
                  args = fArgs SequentialTurnUp
              (ParasitedHost rpath _ r) <- host
              return (Remoted r obj,(rpath, c, u,args,  r)))

  where ssh = binary :: DevOp (Binary "ssh")
        mkOp (_, (rpath, c, u, args, r)) = buildOp
              ("remote-continued: " <> " @" <> pack (show $ resolvedKey r))
              ("calls 'turnup' at host " <> pack (show $ resolvedKey r))
              noCheck
              (do
                  Remote ip <- resolver r
                  blindRun c (sshCmd rpath u ip args) "")
              noAction
              noAction
        sshCmd rpath u ip args  = [ "-o", "StrictHostKeyChecking no"
                                , "-o", "UserKnownHostsFile /dev/null"
                                , "-l", unpack (userName u), unpack ip
                                , remoteExecution rpath args ]
        remoteExecution rpath args = unwords [ "chmod", "+x", rpath, ";", rpath ++ " " ++ unwords args, ";"]
