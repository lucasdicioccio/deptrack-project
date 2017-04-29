{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Devops.Authentication where

import           Data.Monoid             ((<>))
import           Data.String.Conversions (convertString)
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           DepTrack                (declare, track)
import           Prelude                 hiding (readFile)
import           System.FilePath         ((</>))
import           System.IO.Strict        (readFile)

import           Devops.Debian.Commands
import           Devops.Debian.User      (homeDirPath)
import           Devops.Networking
import           Devops.Parasite
import           Devops.Storage
import           Devops.Base

data Private a = Private { getPrivate :: a }
data Public a = Public { getPublic :: a }
type SSHKeyFile = FilePresent
type CertificateFile = FilePresent
data SSHKeyPair = SSHKeyPair { privateKey :: !(Private SSHKeyFile)
                             , publicKey  :: !(Public SSHKeyFile)
                             }
data SSHCertificateAuthority = SSHCertificateAuthority { getCAKeyPair :: !SSHKeyPair }
data SSHSignedUserKey = SSHSignedUserKey { signedKeyPair     :: !SSHKeyPair
                                         , signedCertificate :: !CertificateFile
                                         }

-- | Creates an SSH RSA-2048-bit key.
sshKeyPair :: DevOp DirectoryPresent -> Name -> DevOp (SSHKeyPair)
sshKeyPair dir name = declare (noop "ssh-key" $ "ssh-key:" <> name) $ do
  (DirectoryPresent dirpath) <- dir
  let privKeyPath = dirpath </> Text.unpack name
  -- TODO: pluralize to generatedFiles for multiple-output commands
  (_,_,privPath@(FilePresent priv)) <- generatedFile privKeyPath sshKeygen (mkCommands privKeyPath)
  !pub <- preExistingFile (priv <> ".pub")
  return (SSHKeyPair (Private privPath) (Public pub))
  where mkCommands privKeyPath = return [ "-q"
                     , "-t", "rsa", "-b", "2048"
                     , "-N", ""
                     , "-f", privKeyPath
                     ]

sshCA :: DevOp SSHKeyPair -> DevOp SSHCertificateAuthority
sshCA = fmap SSHCertificateAuthority

signKey :: DevOp SSHCertificateAuthority -> DevOp SSHKeyPair -> DevOp SSHSignedUserKey
signKey ca toSign = track mkOp $ do
  pair@(SSHKeyPair (Private (FilePresent notSignedKeyPath)) _) <- toSign
  let certPath = notSignedKeyPath <> "-cert.pub"
  (_,_,cert) <- generatedFile certPath sshKeygen mkArgs
  return (SSHSignedUserKey pair cert)
  where mkOp (SSHSignedUserKey _ (FilePresent path)) = noop "signed-key" ("certifies " <> Text.pack path)
        mkArgs = do
                (SSHCertificateAuthority (SSHKeyPair caKey _)) <- ca
                (SSHKeyPair _ toCertify) <- toSign
                let (Private (FilePresent caPath)) = caKey
                let (Public (FilePresent toCertifyPath)) = toCertify
                return [ "-s", caPath , "-I", "signedkey" , toCertifyPath ]

data AuthorizedRemote = AuthorizedRemote !ControlledHost !SSHSignedUserKey

authorizedRemote :: DevOp SSHSignedUserKey -> DevOp ControlledHost -> DevOp AuthorizedRemote
authorizedRemote signedKey host = track mkOp $ do
  let privkey = getPrivate . privateKey . signedKeyPair <$> signedKey
  let pubkey  = getPublic . publicKey . signedKeyPair <$> signedKey
  let cert    = signedCertificate <$> signedKey
  (ControlledHost login _) <- host
  _ <- fileTransferred privkey (sshFile login "id_rsa") host
  _ <- fileTransferred pubkey (sshFile login "id_rsa.pub") host
  _ <- fileTransferred cert (sshFile login "id_rsa-cert.pub") host
  AuthorizedRemote <$> host <*> signedKey

  where mkOp (AuthorizedRemote (ControlledHost login (Remote ip)) _) =
           noop ("authorized-remote: " <> login <> "@" <> ip)
                ("Copies a certificate and keys on:"<>ip)

data AuthorizedKeys = AuthorizedKeys {
    authorizedKeys :: [Public SSHKeyFile]
  , authorizedCAs  :: [Public SSHKeyFile]
  }

readTextFile :: FilePath -> IO Text
readTextFile path = convertString <$> readFile path

buildAuthorizedKeysContent :: AuthorizedKeys -> IO FileContent
buildAuthorizedKeysContent aks = do
  let keys = authorizedKeys aks
  let cas = authorizedCAs aks
  keysLines  <- traverse (readTextFile . getFilePresentPath . getPublic) keys
  rawCaLines <- traverse (readTextFile . getFilePresentPath . getPublic) cas
  let caLines = fmap (\dat -> "cert-authority " <> dat) rawCaLines
  -- XXX: Text.concat assumes that .pub files end with new-lines
  let !content = convertString $ Text.concat (keysLines <> caLines)
  return content

-- | Locally builds and sends a .ssh/authorized_keys file for a given user.
-- TODO: explore ways to download public keys from the remote
sendAuthorizedKeys :: FilePath
  -> DevOp (AuthorizedKeys)
  -> DevOp ControlledHost
  -> DevOp (FileTransferred)
sendAuthorizedKeys path keys host = do
  let buildContent = ioFile path (buildAuthorizedKeysContent <$> keys)
  (ControlledHost login _) <- host
  fileTransferred buildContent (sshFile login "authorized_keys") host

-- | Path to a .ssh file for a given user name.
sshFile :: Name -> String -> FilePath
sshFile login x = homeDirPath login </> ".ssh" </> x
