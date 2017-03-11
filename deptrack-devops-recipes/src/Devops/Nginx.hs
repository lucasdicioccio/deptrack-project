{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- TODO:
--   fix pid and run directories
--   disable triggers when debian packages are installed or piggy-back on its /etc configuration
module Devops.Nginx (
    WebService (..)
  , NginxServerConfig (..)
  , NginxLocationConfig (..)
  , NginxLocationDirective (..)
  , proxyPassDirectives
  , staticSiteDirectives
  -- service
  , Nginx
  , ConfigDir
  -- lall the rest
  , nginxWorkerUser
  , nginxMainServer
  , HostName
  ) where

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

-- | A dir where to locate nginx configs.
type ConfigDir = FilePath

-- | A hostname to configure nginx.
type HostName = Name

-- | A WebService. Currently mostly used for passing information to the
-- compiler and as human-friendly documentation.
data WebService = WebService

-- | Opaque type with no inhabitant used for type families when running Nginx
-- with the Devops.Service.daemon function.
data Nginx
type instance DaemonConfig Nginx = FilePresent

-- | Data model of Nginx servers configurations.
data NginxServerConfig =
    NginxServerConfig { _serverPort    :: !(Port (Daemon Nginx))
                      , _siteHostName  :: !HostName
                      , _siteRoot      :: !DirectoryPresent
                      , _siteIndexFile :: !String
                      , _siteLocations :: ![NginxLocationConfig]
                      }

-- | A piece of configuration representing a `location` block in Nginx configs.
data NginxLocationConfig =
    NginxLocationConfig { _locationUrl        :: !FilePath
                        , _locationDirectives :: ![NginxLocationDirective]
                        }

-- | A directive valid in a `location` block.
data NginxLocationDirective =
    NginxProxyPass !String
  | ProxySetHeader !String !String
  | UnspecNginxLocationDirective !String
  | TryFiles !String ![String] !String

-- | Directives for running a reverse proxy to another WebService at a given
-- address.
proxyPassDirectives :: IpNetString
                    -- ^ Address of the proxied service.
                    -> Port WebService
                    -- ^ Port of the proxied service.
                    -> [NginxLocationDirective]
proxyPassDirectives ip port =
    [ NginxProxyPass $ printf "http://%s:%d/" (Text.unpack ip) port
    , ProxySetHeader "Host" "$host"
    , ProxySetHeader "X-Forwarded-For" "$remote_addr"
    ]

-- | Directives for serving a static site.
staticSiteDirectives :: [NginxLocationDirective]
staticSiteDirectives = [ TryFiles "$uri" ["$uri/"] "404" ]

-- | Generates a string for an Nginx config location block directive.
formatDirective :: NginxLocationDirective -> String
formatDirective (NginxProxyPass x)         = printf "proxy_pass %s;" x
formatDirective (ProxySetHeader x y)       = printf "proxy_set_header %s %s;" x y
formatDirective (TryFiles x ys z)          =
    let ys' = unwords ys in printf "try_files %s %s %s;" x ys' z
formatDirective (UnspecNginxLocationDirective x) = x

-- | Generates a representation of a configuration for the main nginx
-- configuration entry point.
mainConfiguration :: User
                  -- ^ The system user nginx runs as.
                  -> [FilePresent]
                  -- ^ Configuration includes.
                  -> ByteString
mainConfiguration (User user) includes =
    ByteString.pack $ unlines [
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
  where
    includeLine (FilePresent path) = printf "include %s;" path

-- | Generates a representation of a server-specific nginx configuration
-- (included from the main one).
serverConfiguration :: NginxServerConfig -> String
serverConfiguration (NginxServerConfig port name (DirectoryPresent rootPath) index locs) =
    unlines [
        "# Generated-file. Do not edit. "
      , "server {"
      , printf "  server_name %s;" (Text.unpack name)
      , printf "  listen %d;" port
      , printf "  root %s;" rootPath
      , printf "  index %s;" index
      , indent $ unlines $ map nginxLocationsConfiguration locs
      , "}"
      ]

-- | Generates a single location (i.e., website) nginx configuration.
nginxLocationsConfiguration :: NginxLocationConfig -> String
nginxLocationsConfiguration (NginxLocationConfig url xs) =
    unlines [
        printf "location %s {" url
      , indent $ unlines $ map formatDirective xs
      , "}"
      ]

-- | Indent all lines of a string one level (hardcoded to two spaces).
indent :: String -> String
indent txt = unlines $ map (\x -> "  " ++ x) $ lines txt

-- | The nginx binary, which must also be stopped to release the default network port.
--
-- Indeed, unfortunately one cannot just prevent apt-get post install hooks
-- from running, hence what is left is to hack around and stop nginx after it
-- started in a post-install hook.
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

-- | We use `www-data` as a system user for nginx workers (the main server runs
-- as super user because it needs extra privileges for binding ports but
-- daemons servicing requests should not have such privileges).
nginxWorkerUser :: DevOp User
nginxWorkerUser = preExistingUser "www-data"

-- | Configures an nginx main server.
--
-- We model an nginx server as a listening daemon.
nginxMainServer :: Port (Daemon Nginx)
                -- ^ port to listen on
                -> ConfigDir
                -- ^ directory for running the server and locating its config
                -> [DevOp NginxServerConfig]
                -- ^ individual sites configuration
                -> DevOp (Listening (Daemon Nginx))
nginxMainServer port rundir mkCfgs =
    let reload = const $ sighupPidFile "/run/nginx.pid" in
    fmap listening $ reloadableDaemon "nginx" Nothing nginx commandArgs reload $ do
      let siteConfigs = traverse generateConfigFile mkCfgs
      let conf = mainConfiguration <$> nginxWorkerUser <*> siteConfigs
      (_,fp) <- fileContent (rundir </> "nginx.conf") conf
      return fp
  where
    listening :: Daemon Nginx -> Listening (Daemon Nginx)
    listening d = Listening port d

    commandArgs :: DaemonConfig Nginx -> CommandArgs
    commandArgs (FilePresent path) = [ "-c" , path ]

    generateConfigFile :: DevOp NginxServerConfig -> DevOp FilePresent
    generateConfigFile mkCfg = fmap snd $ do
        cfg@(NginxServerConfig _ name _ _ _) <- mkCfg
        fileContent (rundir </> printf "nginx-%s.conf" (Text.unpack name))
                    (pure $ convertString $ serverConfiguration cfg)
