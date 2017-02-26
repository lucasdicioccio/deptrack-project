
module Devops.Callback (
    SelfPath
  , MagicArg
  , CallBackMethod (..)
  , ClosureCallBack
  , selfCallback
  ) where

import           Control.Distributed.Closure (Closure)
import qualified Data.Binary                 as Binary
import qualified Data.ByteString.Base64.Lazy as B64
import           Data.String.Conversions (convertString)
import           Data.Typeable (Typeable)

import           Devops.Base

type SelfPath = FilePath
type MagicArg = String

-- | Method to callback a non-local node.
-- TODO: develop on this to be able to represent parasited/chrooted calls
--       ideally we also need a way to "link" long-lived processes such as Backends together
data CallBackMethod = BinaryCall !FilePath ![String]

-- | Function to build a callback to a Closure of a DevOp.
type ClosureCallBack a = Closure (DevOp a) -> DevOp CallBackMethod

-- | Creates a callback to self using a magic argument for branching at the main.
--
-- The second argument will be a base-64-encoded serialization of the closure
-- callback.
selfCallback :: Typeable a => SelfPath -> MagicArg -> ClosureCallBack a
selfCallback self magicArg = \clo -> do
    let b64data = convertString $ B64.encode $ Binary.encode clo
    return $ BinaryCall self (magicArg:[b64data])
