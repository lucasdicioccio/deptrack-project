{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- TODO:
--   fix pid and run directories
--   disable triggers when debian packages are installed or piggy-back on its /etc configuration
module Devops.Nginx where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as ByteString
import           Data.String.Conversions (convertString)
import qualified Data.Text               as Text
import           System.FilePath.Posix   ((</>))
import           Text.Printf             (printf)

import           Devops.Binary           (Binary, bin, binary)
import           Devops.Debian           (installedWith, postInstallHook)
import qualified Devops.Debian.Packages  as Pkg
import           Devops.Debian.User      (User (..), preExistingUser)
import           Devops.Networking
import           Devops.Service
import           Devops.Storage
import           Devops.Base
import           Devops.Utils            (blindRun)

type RunDir = FilePath
type HostName = Name
type UrlPart = String
data WebService = WebService

data NginxServerConfig =
  NginxServerConfig !(Port (Daemon Nginx)) !HostName !DirectoryPresent !String ![NginxLocationConfig]

server :: Port (Daemon Nginx)
       -> HostName
       -> DirectoryPresent
       -> String
       -> [NginxLocationConfig]
       -> NginxServerConfig
server = NginxServerConfig

data NginxLocationConfig =
  NginxLocationConfig !FilePath ![NginxLocationDirective]

location :: UrlPart -> [NginxLocationDirective] -> NginxLocationConfig
location = NginxLocationConfig

data NginxLocationDirective =
    NginxProxyPass !String
  | UnspecNginxLocationDirective !String

proxyPassDirectives :: IpNetString -> Port WebService -> [NginxLocationDirective]
proxyPassDirectives ip port =
  [ NginxProxyPass $ printf "http://%s:%d/" (Text.unpack ip) port
  , UnspecNginxLocationDirective "proxy_set_header Host            $host;"
  , UnspecNginxLocationDirective "proxy_set_header X-Forwarded-For $remote_addr;"
  ]

staticSiteDirectives :: [NginxLocationDirective]
staticSiteDirectives = [ UnspecNginxLocationDirective "try_files $uri $uri/ =404;" ]

getDirective :: NginxLocationDirective -> String
getDirective (NginxProxyPass x)         = printf "proxy_pass %s;" x
getDirective (UnspecNginxLocationDirective x) = x

data Nginx
type instance DaemonConfig Nginx = (FilePresent, Port (Daemon Nginx))

nginxCommandArgs :: DaemonConfig Nginx -> CommandArgs
nginxCommandArgs (FilePresent path,_) = ["-c", path]

mainConfiguration :: User -> [FilePresent] -> ByteString
mainConfiguration (User user) includes = ByteString.pack $ unlines [
    printf "user %s;" (Text.unpack user)
  , "daemon off;"
  , "worker_processes 4;"
  , "pid /run/nginx.pid;"
  , ""
  , "events {"
  , "  worker_connections 512;"
  , "}"
  , ""
  , "http {"
  , "  sendfile on;"
  , "  tcp_nopush on;"
  , "  tcp_nodelay on;"
  , "  keepalive_timeout 65;"
  , ""
  , "  types_hash_max_size 2048;"
  , "  include /etc/nginx/mime.types;"
  , "  default_type application/octet-stream;"
  , ""
  , "  access_log /var/log/nginx/access.log;"
  , "  error_log /var/log/nginx/error.log;"
  , ""
  , "  gzip on;"
  , "  gzip_disable \"msie6\";"
  , ""
  , indent $ unlines $ map includeLine $ includes
  , "}"
  ]

  where includeLine (FilePresent path) = printf "include %s;" path

serverConfiguration :: NginxServerConfig -> String
serverConfiguration (NginxServerConfig port name (DirectoryPresent rootPath) index locs) = unlines [
    "# Generated-file. Do not edit. "
  , "server {"
  , printf "  server_name %s;" (Text.unpack name)
  , printf "  listen %d;" port
  , printf "  root %s;" rootPath
  , printf "  index %s;" index
  , indent $ unlines $ map nginxLocationsConfiguration locs
  , "}"
  ]

nginxLocationsConfiguration :: NginxLocationConfig -> String
nginxLocationsConfiguration (NginxLocationConfig url xs) = unlines [
    printf "location %s {" url
  , indent $ unlines $ map getDirective xs
  , "}"
  ]

indent :: String -> String
indent txt = unlines $ map (\x -> "  " ++ x) $ lines txt

nginx :: DevOp (Binary "/usr/sbin/nginx")
nginx = binary `installedWith` postInstallHook stopSystemdService Pkg.nginx
  where
    stopSystemdService :: PreOp
    stopSystemdService = buildPreOp ("stop-service: nginx")
                                ("stop existing systemd nginx service that wants port 80")
                                noCheck
                                (blindRun systemdServiceBin ["nginx", "stop"] "")
                                (noAction)
                                (noAction)
    systemdServiceBin :: Binary "service"
    systemdServiceBin = bin "/usr/sbin/service"

nginxWorkerUser :: DevOp User
nginxWorkerUser = preExistingUser "www-data" -- installed with the package

nginxMainServer :: RunDir
                -> [DevOp NginxServerConfig]
                -> DevOp (Listening (Daemon Nginx))
nginxMainServer rundir mkCfgs = fmap listeningNginxDaemon $ reloadableDaemon "nginx" Nothing nginx nginxCommandArgs (const $ sighupPidFile "/run/nginx.pid") $ do
  let siteConfigs = traverse (nginxServerConfig rundir) mkCfgs
  (_,fp) <- fileContent (rundir </> "nginx.conf")
                        (mainConfiguration <$> nginxWorkerUser <*> siteConfigs)
  return (fp,80)

nginxServerConfig :: RunDir -> DevOp NginxServerConfig -> DevOp (FilePresent)
nginxServerConfig rundir mkCfg = fmap snd $ do
  cfg@(NginxServerConfig _ name _ _ _) <- mkCfg
  fileContent (rundir </> printf "nginx-%s.conf" (Text.unpack name))
              (pure $ convertString $ serverConfiguration cfg)

listeningNginxDaemon :: Daemon Nginx -> Listening (Daemon Nginx)
listeningNginxDaemon d@(Daemon _ _ _ (_,port)) = Listening port d
