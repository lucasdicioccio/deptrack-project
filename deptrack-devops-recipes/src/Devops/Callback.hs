{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

module Devops.Callback (
    SelfPath
  , MagicArg
  , BinaryCall (..)
  , binaryCall
  , Continued
  , continue
  , continueConst
  , eval
  , callback
  ) where

import           Devops.Base
import           Devops.Cli

type SelfPath = FilePath
type MagicArg = String

-- | Method to callback a non-local node.
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

continueConst :: DevOp a -> BinaryCall -> Continued a
continueConst tgt call =
    let obj = ()
        fDevop () = tgt
        fCall () = call
    in continue obj fDevop fCall

eval :: Continued a -> a
eval (Continued arg f _) = runDevOp $ f arg

callback :: Continued a -> BinaryCall
callback (Continued arg _ g) = g arg
