{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RankNTypes        #-}

module Devops.App where

import           GHC.TypeLits (Symbol, KnownSymbol)

import           Devops.Base
import           Devops.Binary
import           Devops.Debian.User (User, Group)
import           Devops.Utils
import           Devops.Service
import           Devops.Storage

-- | An runtime-based App tagged with symbols.
--
-- Although one could only use Binary and artifacts for running runtime-based
-- applications (e.g., Java, DotNet) or frameworks (e.g., Rails), the feature
-- of having a runtime an application is common enough to warrant some extra
-- typing and unifying functions/typeclasses.
data App (runtime :: Symbol) (b :: Symbol) =
    App { appBinary :: !(Binary runtime)
        , appName   :: !Name
        , appDir    :: !DirectoryPresent
        }

app :: Name -> Binary a -> DirectoryPresent -> App a b
app n b path = App b n path

-- | An application for which information is passed as type arguments.
application :: Name -> DevOp env (Binary a) -> DevOp env DirectoryPresent -> DevOp env (App a b)
application proj runtime dir = app proj <$> runtime <*> dir

-- | A command for an application.
type AppCommand runtime a = App runtime a -> [String]

-- | Runs a command in an applcation.
runApp :: App a b -> AppCommand a b -> IO ()
runApp a cmd =
    blindRunInDir (appBinary a) (getDirectoryPresentPath $ appDir a) (cmd a) ""

type instance DaemonConfig (App a b) = (App a b, AppCommand a b)

daemonizeApp :: (KnownSymbol a, KnownSymbol b)
             => Name
             -> DevOp env (App a b)
             -> AppCommand a b
             -> Maybe (DevOp env (User, Group))
             -> DevOp env (Daemon (App a b))
daemonizeApp name mkApp cmd mkUserGroup = do
    let mkBinary = fmap appBinary mkApp
    daemon name mkUserGroup mkBinary (\(x,f) -> f x) ((,) <$> mkApp <*> return cmd)
