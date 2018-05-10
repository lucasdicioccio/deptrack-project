{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Devops.Dhcp where

import qualified Data.ByteString       as ByteString
import           Data.Monoid             ((<>))
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           System.FilePath.Posix   ((</>))
import           Text.Printf             (PrintfType, printf)

import           Devops.Apparmor
import           Devops.Debian.Commands
import           Devops.DnsResolver
import           Devops.Networking
import           Devops.Service
import           Devops.Storage
import           Devops.Base

printt :: PrintfType r => Text -> r
printt = printf . Text.unpack

data DHCP
type instance DaemonConfig DHCP = (FilePresent, Interface)

-- | Basic DHCP Configuration.
dhcpConfiguration :: AddressPlan -> Remoted (Listening DnsService) -> Text
dhcpConfiguration plan (Remoted dns _) =
    Text.unlines $ [ header , "" , body ]
  where
    dnsIp :: IpNetString
    dnsIp = remoteIp $ dns

    header :: Text
    header = Text.unlines $ [ "option domain-name-servers "<> dnsIp <>";"
                            , "option domain-name \"localhost.\";"
                            , "option domain-search \"localhost\";"
                            , "default-lease-time 86400;"
                            , "max-lease-time 604800;"
                            , "authoritative;"
                            ]

    body ::Text
    body = Text.unlines [ "subnet " <> zerothIP plan <> " netmask " <> netmaskIpStr plan <> " {"
                        , options
                        , hosts
                        , "}"
                        ]

    options :: Text
    options = Text.unlines $ [ "option subnet-mask " <> netmaskIpStr plan <> ";"
                             , "option broadcast-address "<> broadcastIpStr plan <>";"
                             , "option routers "<> localhostIp plan <>";"
                             ]
    hosts :: Text
    hosts = Text.unlines $ map host (indices plan)

    host :: Index -> Text
    host idx = let hostname = convertString $ fixedHostName plan idx
                   mac      = convertString $ fixedMac plan idx
                   ip       = convertString $ fixedIp plan idx
               in Text.unlines [ "host "<>hostname<>" {"
                               , "hardware ethernet " <> mac <> ";"
                               , "fixed-address " <> ip <> ";"
                               , "option host-name \"" <> hostname <> "\";"
                               , "option domain-name \"local\";"
                               , "}"
                               ]

-- | An apparmor include config file allowing dhcpd to read its configuration.
apparmorConfig :: FilePresent -> DevOp FilePresent
apparmorConfig (FilePresent pathToDhcpConfig) = do
    let apparmorConfigPath = "/etc/apparmor.d/dhcpd.d/devop-config"
    let mainCfgPath = "/etc/apparmor.d/usr.sbin.dhcpd"
    let dat = convertString $ apparmorContent pathToDhcpConfig
    _ <- fileContent apparmorConfigPath (pure dat)
    return $ FilePresent mainCfgPath
  where
    apparmorContent :: FilePath -> Text
    apparmorContent confPath = Text.unlines [ Text.pack confPath <> " r," ]

dhcpCommanArgs :: DaemonConfig DHCP -> CommandArgs
dhcpCommanArgs (FilePresent path, iface) =
  ["-f", "-cf", path, Text.unpack $ interfaceName iface]

-- | Deletes the leases and leases~ files because they sometime hurt after a
-- crash+restart.
trashDhcpLeases :: DevOp ()
trashDhcpLeases = declare mkOp (return ())
  where
    mkOp = buildPreOp ("trash-dhcp-leases") ("deletes the dhcp-leases file")
                   noCheck
                   (blindRemoveLink "/var/lib/dhcp/dhcpd.leases"
                   >> blindRemoveLink "/var/lib/dhcp/dhcpd.leases~"
                   >> ByteString.writeFile "/var/lib/dhcp/dhcpd.leases" "")
                   noAction
                   noAction

-- | Spawns the server.
dhcpServer :: RunDir
           -> AddressPlan
           -> DevOp (Remoted (Listening DnsService))
           -> DevOp Bridge
           -> DevOp (Daemon DHCP)
dhcpServer rundir plan mkDns br = daemon "dhcpd" Nothing dhcpd dhcpCommanArgs $ do
  dns <- mkDns
  let (!txtConfig) = convertString (dhcpConfiguration plan dns)
  (_,fp) <- fileContent (rundir </> "dhcp-alt.conf") (pure $ txtConfig)
  _ <- reloadApparmor (apparmorConfig fp)
  _ <- trashDhcpLeases
  let ipnet = ipNetString (localhostIp plan) (prefixLenString plan)
  (ConfiguredInterface _ iface) <- (configuredInterface ipnet (BridgeIface <$> br) binIp)
  return (fp,iface)
