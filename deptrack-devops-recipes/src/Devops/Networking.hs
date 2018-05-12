{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Devops.Networking (
    Interface (..) , interfaceName
  , ConfiguredInterface (..) , configuredInterface
  , IpNetString , Port
  , MacAddressString
  , HostString
  , Bridge , bridge
  , Tap , tap
  , BridgedInterface , bridgedInterface
  , ipForwarding
  , nating
  --
  , Index , AddressPlan (..)
  , zerothIP , broadcastIpStr , netmaskIpStr , prefixLenString
  , ipNetString
  , TenspaceIndex , tenspaceAddressPlan
  --
  , Remote (..)
  , Remoted (..)
  , Proxied (..) , Listening (..) , Exposed (..)
  , publicFacingService
  , proxyRemote , publicFacingProxy
  , existingRemote
  , exposed2listening
  --
  , TTL
  ) where

import           Data.Monoid             ((<>))
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Typeable (Typeable)
import           Prelude                 hiding (readFile)
import           System.IO.Strict        (readFile)
import           Text.Printf             (printf)

import           Devops.Binary
import           Devops.Debian.Commands
import           Devops.Debian.User      (User (..))
import           Devops.Base
import           Devops.Utils

type Index = Int

-- | An IP-Addressing plan for a local subnet tree.
-- Currently assumes a "/24" prefix.
data AddressPlan = AddressPlan { indices         :: [Index]
                               , localhostIp     :: IpNetString
                               , localNetwork    :: IpNetString
                               , fixedMac        :: Index -> MacAddressString
                               , fixedIp         :: Index -> IpNetString
                               , fixedHostName   :: Index -> HostString
                               , fixed0xHostName :: Index -> HostString
                               }

ipNetString :: Text -> Text -> IpNetString
ipNetString ip pfx = ip <> "/" <> pfx

zerothIP :: AddressPlan -> IpNetString
zerothIP = Text.takeWhile ((/=)'/') . localNetwork

-- | WARNING only works on /24 !!
broadcastIpStr :: AddressPlan -> IpNetString
broadcastIpStr plan =
    let z = zerothIP plan in
    let txtPfx = take 3 (Text.splitOn "." z) in
    Text.intercalate "." (txtPfx ++ ["255"])

netmaskIpStr :: AddressPlan -> IpNetString
netmaskIpStr _ = "255.255.255.0"

prefixLenString :: AddressPlan -> Text
prefixLenString = Text.drop 1 . Text.dropWhile ((/=)'/') . localNetwork

-- | An index representing a 10-space network.
-- Should be in [1..254]
type TenspaceIndex = Index

-- | A default address plan allowing hosts on the 10.1.1.0\24 range.
tenspaceAddressPlan :: TenspaceIndex -> AddressPlan
tenspaceAddressPlan n | n < 1 || n > 254 = error "n should be in [1..254]"
tenspaceAddressPlan n =
    AddressPlan [2..254] localIp localNet fmac fip fhost f0xhost
  where
    localIp :: IpNetString
    localIp = Text.pack $ "10." <> show n <> ".1.1"
    localNet :: IpNetString
    localNet = Text.pack $ "10." <> show n <> ".1.0/24"
    fmac :: Index -> MacAddressString
    fmac idx | idx < 255 = Text.pack $ printf "52:54:%02x:22:33:%02x" n idx
             | otherwise = error $ printf "invalid index for MAC address: %d" idx
    fip :: Index -> IpNetString
    fip idx | idx < 255 = Text.pack $ printf "10.%d.1.%d" n idx
            | otherwise = error $ printf "invalid index for IP address: %d" idx
    fhost :: Index -> HostString
    fhost idx = Text.pack $ printf "vm%03d" idx
    f0xhost :: Index -> HostString
    f0xhost idx = Text.pack $ printf "vm0x%02x" idx

data IPForwarding = IPForwarding
data NATing = NATing !Interface !Interface -- from/to
data Bridge = Bridge !Name
data Tap = Tap !Name !User
data Interface = TapIface !Tap | BridgeIface !Bridge | PhysicalIface !Name | OtherInetIface !Name
data ConfiguredInterface = ConfiguredInterface !IpNetString !Interface

interfaceName :: Interface -> Name
interfaceName (TapIface (Tap n _)) = n
interfaceName (BridgeIface (Bridge n)) = n
interfaceName (PhysicalIface n) = n
interfaceName (OtherInetIface n) = n

type BridgedInterface = (Interface, Bridge)
type IpNetString = Text
type MacAddressString = Text
type Port a = Int -- Port exposing a given service of type a.
type HostString = Text

-- | A remote host.
data Remote = Remote { remoteIp :: !IpNetString }
-- | A value on a remote.
data Remoted a = Remoted !Remote !a
  deriving (Functor, Foldable, Traversable)
-- | A proxied service.
data Proxied a = Proxied { proxiedServiceInfo      :: !(IpNetString,Port a)
                         , proxiedServiceLocalPort :: !(Port a)
                         , proxiedService          :: !a
                         } deriving Functor

-- | A service listening on a local port.
data Listening a = Listening { listeningPort :: !(Port a) , listeningService :: !a }
  deriving Functor

-- | A service exposed on a local port.
data Exposed a = Exposed { exposedPort :: !(Port a) , exposedService :: !a }
  deriving Functor

-- | Builds an existing remote.
existingRemote :: IpNetString -> DevOp env Remote
existingRemote ip = declare mkOp (pure $ Remote ip)
  where mkOp = noop ("existing-remote: " <> ip)
                    ("an already-existing network host")

ipForwardingProcPath :: FilePath
ipForwardingProcPath = "/proc/sys/net/ipv4/ip_forward"

isForwardingEnabled :: IO Bool
isForwardingEnabled = (=="1\n") <$> readFile ipForwardingProcPath

enableForwarding,disableForwarding :: IO ()
enableForwarding = writeFile ipForwardingProcPath "1\n"
disableForwarding = writeFile ipForwardingProcPath "0\n"

ipForwarding :: DevOp env IPForwarding
ipForwarding =
  let mkop _ = buildOp ("linux-ip-forwarding")
                       ("ensures that /proc/sys/net/ipv4/ip_forward is 1")
                       (fromBool <$> isForwardingEnabled)
                       (enableForwarding)
                       (disableForwarding)
                       noAction
  in devop id mkop (pure IPForwarding)

-- Unsatisfying NAT configuration using iptables.
-- The main problem is that iptables rules are a global shared value and order matters.
-- We should break-down individual needs (e.g., nat = forward related in,
-- forward everything out etc.). Also, use a graph-optimization to collect,
-- merge, and atomically commit rules.
nating :: DevOp env (Binary "iptables")
       -> DevOp env Interface
       -> DevOp env Interface
       -> DevOp env (NATing, Binary "iptables")
nating binaries internet lan = devop id mkOp $ do
    nat <- NATing <$> internet <*> lan
    bins <- binaries
    return (nat, bins)
  where
    mkOp ((NATing a b), i) = buildOp
            ("linux-ip-network-address-translation: " <> interfaceName b <> " via " <> interfaceName a)
            ("ensures that Linux NAT is enabled")
            (checkBinaryExitCodeAndStdout null i [
                     "-w"
                   , "-t", "nat"
                   , "-C", "POSTROUTING"
                   , "-o", Text.unpack $ interfaceName a
                   , "-j", "MASQUERADE" ] "")
            (blindRun i [
                     "-w"
                   , "-t", "nat"
                   , "-I", "POSTROUTING"
                   , "-o", Text.unpack $ interfaceName a
                   , "-j", "MASQUERADE" ] ""
             >> blindRun i [
                     "-w"
                   , "-I", "FORWARD"
                   , "-i", Text.unpack $ interfaceName b
                   , "-o", Text.unpack $ interfaceName a
                   , "-j", "ACCEPT" ] ""
             >> blindRun i [
                     "-w"
                   , "-I", "FORWARD"
                   , "-i", Text.unpack $ interfaceName a
                   , "-o", Text.unpack $ interfaceName b
                   , "-m", "state"
                   , "--state", "RELATED,ESTABLISHED"
                   , "-j", "ACCEPT" ] ""
            )
            (blindRun i [
                     "-w"
                   , "-t", "nat"
                   , "-D", "POSTROUTING"
                   , "-o", Text.unpack $ interfaceName a
                   , "-j", "MASQUERADE" ] ""
             >> blindRun i [
                     "-w"
                   , "-D", "FORWARD"
                   , "-i", Text.unpack $ interfaceName b
                   , "-o", Text.unpack $ interfaceName a
                   , "-j", "ACCEPT" ] ""
             >> blindRun i [
                     "-w"
                   , "-D", "FORWARD"
                   , "-i", Text.unpack $ interfaceName a
                   , "-o", Text.unpack $ interfaceName b
                   , "-m", "state"
                   , "--state", "RELATED,ESTABLISHED"
                   , "-j", "ACCEPT" ] ""
            )
            noAction

bridge :: Name -> DevOp env (Binary "brctl", Binary "ip") -> DevOp env (Bridge, (Binary "brctl", Binary "ip"))
bridge name binaries = devop id mkop $ do
    let br = Bridge name
    bins <- binaries
    return (br, bins)
  where
    mkop (_,(b,ip)) = buildOp ("linux-ip-network-bridge: " <> name)
                     ("ensures that softbridge " <> name <> " exists")
                     (checkExitCode "ip" ["addr", "show", Text.unpack name] "")
                     (blindRun b ["addbr", Text.unpack name] ""
                      >> blindRun ip ["link", "set", "dev", Text.unpack name, "up"] "")
                     (blindRun ip ["link", "set", "dev", Text.unpack name, "down"] ""
                      >> blindRun b ["delbr", Text.unpack name] "")
                     noAction


tap :: Name -> DevOp env User -> DevOp env (Binary "tunctl") -> DevOp env (Tap, Binary "tunctl")
tap name usr binaries = devop id mkOp $ do
    (,) <$> (Tap name <$> usr) <*> binaries 
  where
    mkOp (Tap _ (User u), t) = buildOp
        ("linux-ip-network-tap: " <> name)
        ("ensures that tap interface " <> name <> " belongs to user " <> convertString u)
        (checkExitCode "ip" ["addr", "show", Text.unpack name] "")
        (blindRun t ["-u", Text.unpack u, "-t", Text.unpack name] "")
        (blindRun t ["-d", Text.unpack name] "")
        noAction

configuredInterface :: IpNetString -> DevOp env Interface -> DevOp env (Binary "ip") -> DevOp env (ConfiguredInterface)
configuredInterface addrStr mkIface binaries = devop fst mkOp $ do
   iface <- ConfiguredInterface addrStr <$> mkIface
   bins <- binaries
   return (iface, bins)
  where
    mkOp ((ConfiguredInterface _ iface), ip) = buildOp
            ("linux-ip-network-set:" <> interfaceName iface <> " as " <> addrStr)
            ("ensures that an interface has an IP")
            noCheck
            (blindRun ip ["addr", "add", Text.unpack addrStr, "dev", Text.unpack $ interfaceName iface] "")
            (blindRun ip ["addr", "del", Text.unpack addrStr, "dev", Text.unpack $ interfaceName iface] "")
            noAction

bridgedInterface :: DevOp env Interface -> DevOp env Bridge -> DevOp env (Binary "brctl", Binary "ip") -> DevOp env (Interface, Bridge, (Binary "brctl", Binary "ip"))
bridgedInterface mkIface mkBridge binaries =
  let mkOp ((TapIface (Tap n _)), Bridge br, (b,ip)) = buildOp
        ("linux-ip-network-bridged-tap: " <> n <> "<->" <> br)
        ("ensures that a tap interface is bridged")
        noCheck
        (blindRun b ["addif", Text.unpack br, Text.unpack n] ""
         >> blindRun ip ["link", "set", "dev", Text.unpack n, "up", "promisc", "on"] "")
        (blindRun ip ["link", "set", "dev", Text.unpack n, "down"] ""
         >> blindRun b ["delif", Text.unpack br, Text.unpack n] "")
        noAction
      mkOp _ = error "only knows how to bridge a TapIface"
  in devop id mkOp ((,,) <$> mkIface <*> mkBridge <*> binaries)

-- | Proxies and forward traffic from a local port to a remotely-listening service.
proxyRemote :: Typeable a
            => Port a
            -> DevOp env ConfiguredInterface
            -> DevOp env (Remoted (Listening a))
            -> DevOp env (Proxied a)
proxyRemote publicPort mkInternet mkRemote = devop snd mkOp $ do
    (Remoted (Remote ip) (Listening machinePort x)) <- mkRemote
    iptablesCommand <- iptables
    internet <- mkInternet
    return ((iptablesCommand, internet), Proxied (ip, machinePort) publicPort x)
  where
    mkOp ((i, (ConfiguredInterface publicIp a)), Proxied (ip, machinePort) _ _) =
               buildOp (Text.pack $ printf "proxied-service: %s:%d to %s:%d" (Text.unpack publicIp) publicPort (Text.unpack ip) machinePort)
                  ("setups port-mapping between public ip/port and private ip/port")
                  (checkBinaryExitCodeAndStdout null i [
                     "-w"
                   , "-t", "nat"
                   , "-C", "PREROUTING"
                   , "-i", Text.unpack $ interfaceName a
                   , "-p", "tcp", "--dport", show publicPort
                   , "-j", "DNAT"
                   , "--to", printf "%s:%d" (Text.unpack ip) machinePort ] "")
                  (blindRun i [
                     "-w"
                   , "-t", "nat"
                   , "-I", "POSTROUTING"
                   , "-o", Text.unpack $ interfaceName a
                   , "-p", "tcp", "--sport", show machinePort
                   , "-j", "SNAT"
                   , "--to", Text.unpack publicIp ] ""
                  >> blindRun i [
                     "-w"
                   , "-t", "nat"
                   , "-I", "PREROUTING"
                   , "-i", Text.unpack $ interfaceName a
                   , "-p", "tcp", "--dport", show publicPort
                   , "-j", "DNAT"
                   , "--to", printf "%s:%d" (Text.unpack ip) machinePort ] ""
                  )
                  (blindRun i [
                     "-w"
                   , "-t", "nat"
                   , "-D", "POSTROUTING"
                   , "-o", Text.unpack $ interfaceName a
                   , "-p", "tcp", "--sport", show machinePort
                   , "-j", "SNAT"
                   , "--to", Text.unpack $ publicIp ] ""
                  >> blindRun i [
                     "-w"
                   , "-t", "nat"
                   , "-D", "PREROUTING"
                   , "-i", Text.unpack $ interfaceName a
                   , "-p", "tcp", "--dport", show publicPort
                   , "-j", "DNAT"
                   , "--to", printf "%s:%d" (Text.unpack ip) machinePort ] ""
                  )
                  noAction

-- | Exposes a proxied service to the Internet.
--  * Listening service is proxied -> private port is used because prerouting rewrites ports
publicFacingProxy :: Typeable a => DevOp env (Proxied a) -> DevOp env (Exposed a)
publicFacingProxy mkProxy = devop snd mkOp $ do
    (Proxied (_,privatePort) port val) <- mkProxy
    iptablesCommand <- iptables
    return ((iptablesCommand,privatePort), Exposed port val)
  where
    mkOp ((i,port),(Exposed publicPort _)) =
               buildOp (Text.pack $ printf "exposed-port: %d" publicPort)
                  ("opens port on the firewall")
                  (checkBinaryExitCodeAndStdout null i [
                     "-w"
                   , "-C", "FORWARD"
                   , "-p", "tcp", "--dport", show port
                   , "-j", "ACCEPT" ] "")
                  (blindRun i [
                     "-w"
                   , "-I", "FORWARD"
                   , "-p", "tcp", "--dport", show port
                   , "-j", "ACCEPT" ] "")
                  (blindRun i [
                     "-w"
                   , "-D", "FORWARD"
                   , "-p", "tcp", "--dport", show port
                   , "-j", "ACCEPT" ] "")
                  noAction

--  * Listening service is open locally -> should be using the INPUT chain.
publicFacingService :: Typeable a => DevOp env (Listening a) -> DevOp env (Exposed a)
publicFacingService mkListening = devop snd mkOp $ do
  (Listening port val) <- mkListening
  iptablesCommand <- iptables
  return (iptablesCommand, Exposed port val)
  where mkOp (i,(Exposed port _)) =
               buildOp (Text.pack $ printf "exposed-port: %d" port)
                  ("opens port on the firewall")
                  (checkBinaryExitCodeAndStdout null i [
                     "-w"
                   , "-C", "INPUT"
                   , "-p", "tcp", "--dport", show port
                   , "-j", "ACCEPT" ] "")
                  (blindRun i [
                     "-w"
                   , "-I", "INPUT"
                   , "-p", "tcp", "--dport", show port
                   , "-j", "ACCEPT" ] "")
                  (blindRun i [
                     "-w"
                   , "-D", "INPUT"
                   , "-p", "tcp", "--dport", show port
                   , "-j", "ACCEPT" ] "")
                  noAction

exposed2listening :: Exposed a -> Listening a
exposed2listening (Exposed a b) = Listening a b

type TTL = Int
