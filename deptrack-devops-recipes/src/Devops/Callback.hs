{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

module Devops.Callback (
    SelfPath
  , MagicArg
  , BinaryCall (..)
  , binaryCall
  , Continued
  , continue
  , eval
  , callback
  ) where

import           Devops.Base
import           Devops.Cli

type SelfPath = FilePath
type MagicArg = String

-- | Method to callback a non-local node.
-- TODO: develop on this to be able to represent parasited/chrooted calls
--       ideally we also need a way to "link" long-lived processes such as Backends together
data BinaryCall = BinaryCall {
    _callbackBinaryPath :: !FilePath
  , _callbackArgs       :: !(Method -> [String])
  }

binaryCall :: FilePath -> (Method -> [String]) -> BinaryCall
binaryCall = BinaryCall

data Continued a = forall obj. Continued {
    _arg        :: obj
  , _mkDevOp    :: obj -> DevOp a
  , _mkCallback :: obj -> BinaryCall
  }

continue :: obj
         -- ^ A value.
         -> (obj -> DevOp a)
         -- ^ A function to build a DevOp.
         -> (obj -> BinaryCall)
         -- ^ A function to build a suitable callback.
         -> Continued a
continue = Continued

eval :: Continued a -> a
eval (Continued arg f _) = runDevOp $ f arg

callback :: Continued a -> BinaryCall
callback (Continued arg _ g) = g arg
