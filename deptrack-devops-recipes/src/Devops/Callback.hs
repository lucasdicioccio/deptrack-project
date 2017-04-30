{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

module Devops.Callback (
    SelfPath
  , MagicArg
  , CallBackMethod (..)
  , Continued
  , continue
  , eval
  , callback
  , ClosureCallBack
  , selfClosureCallback
  , continueClosure
  ) where

import           Control.Distributed.Closure (Closure, unclosure)
import qualified Data.Binary                 as Binary
import qualified Data.ByteString.Base64.Lazy as B64
import           Data.String.Conversions (convertString)
import           Data.Typeable (Typeable)

import           Devops.Base
import           Devops.Cli

type SelfPath = FilePath
type MagicArg = String

-- | Method to callback a non-local node.
-- TODO: develop on this to be able to represent parasited/chrooted calls
--       ideally we also need a way to "link" long-lived processes such as Backends together
data CallBackMethod = BinaryCall !FilePath !(Method -> [String])

data Continued a = forall obj. Continued {
    _arg        :: obj
  , _mkDevOp    :: obj -> DevOp a
  , _mkCallback :: obj -> DevOp CallBackMethod
  }

continue :: obj
         -- ^ A value.
         -> (obj -> DevOp a)
         -- ^ A function to build a DevOp.
         -> (obj -> DevOp CallBackMethod)
         -- ^ A function to build a suitable callback.
         -> Continued a
continue = Continued

eval :: Continued a -> a
eval (Continued arg f _) = runDevOp $ f arg

callback :: Continued a -> DevOp CallBackMethod
callback (Continued arg _ g) = g arg

-- | Function to build a callback to a Closure of a DevOp.
type ClosureCallBack = forall a. Typeable a => Closure (DevOp a) -> DevOp CallBackMethod

-- | Creates a callback to self using a magic argument for branching at the main.
--
-- The second argument will be a base-64-encoded serialization of the closure
-- callback.
selfClosureCallback :: SelfPath -> MagicArg -> ClosureCallBack
selfClosureCallback self magicArg = \clo -> do
    let b64data = convertString $ B64.encode $ Binary.encode clo
    return $ BinaryCall self (const $ magicArg:[b64data])

-- | You should import this function only in leaf code rather than library code.
continueClosure :: Typeable a
                => Closure (DevOp a)
                -- ^ A closure you want to serialize.
                -> ClosureCallBack
                -- ^ An encoder for the closure.
                -> Continued a
continueClosure clo mkCb = continue clo unclosure mkCb
