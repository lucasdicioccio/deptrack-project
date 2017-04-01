{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}

module Devops.App where

import           Data.Proxy   (Proxy (..))
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import           Devops.Base
import           Devops.Binary
import           Devops.Utils

-- | An runtime-based App tagged with symbols.
--
-- Although one could only use Binary and artifacts for running runtime-based
-- applications (e.g., Java, DotNet) or frameworks (e.g., Rails), the feature
-- of having a runtime an application is common enough to warrant some extra
-- typing and unifying functions/typeclasses.
data App (runtime :: Symbol) (b :: Symbol) = App (Binary runtime) !FilePath

app :: Binary a -> FilePath -> App a b
app b path = App b path

appPath :: App a b -> FilePath
appPath (App _ x) = x

appBinary :: App a b -> Binary a
appBinary (App a _) = a

-- | An application for which information is passed as type arguments.
application :: (KnownSymbol b) => DevOp (Binary a) -> DevOp (App a b)
application runtime = f Proxy <$> runtime
  where
    f :: (KnownSymbol b) => Proxy b -> Binary a -> App a b
    f proxy b = app b (symbolVal proxy)

-- | A command for an application.
type AppCommand (a :: Symbol) = FilePath -> [String]

-- | Runs a command in an applcation.
runApp :: App a b -> AppCommand a -> IO ()
runApp a cmd =
    blindRunInDir (appBinary a) (appPath a) (cmd $ appPath a) ""
