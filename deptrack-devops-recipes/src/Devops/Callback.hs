{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

module Devops.Callback (
    BinaryCall (..)
  , binaryCall
  , Continued
  , continue
  , continueConst
  , eval
  , callback
  ) where

import           Devops.Base
import           Devops.Cli

-- | Method to callback a non-local node.
data BinaryCall = BinaryCall {
    _callbackBinaryPath :: !FilePath
  -- ^ The callback to a binary that will understand '_callbackArgs'.
  , _callbackArgs       :: !(Method -> [String])
  -- ^ A function to build arguments for a given 'Method'.
  }

-- | Constructs a 'BinaryCall'.
binaryCall :: FilePath -> (Method -> [String]) -> BinaryCall
binaryCall = BinaryCall

-- | Represents a continuation-based computation where we can both:
-- - build a DevOp with '_mkDevOp'
-- - compute a BinaryCall with '_mkCallback', the BinaryCall should be another
-- DepTrack program that will apply the currently-applying method on a DevOp
-- semantically-equivalent to the one which is built with '_mkDevOp'
--
-- The implementation uses an ExistentialQuantification over the continuation
-- argument to show that there is no other option but to apply either of
-- '_mkDevOp' or '_mkCallback' to make progress.
--
-- This object is useful to capture DevOp that must be run on a remote or
-- containerized machine.
data Continued env a = forall obj. Continued {
    _arg        :: obj
  , _mkDevOp    :: obj -> DevOp env a
  , _mkCallback :: obj -> BinaryCall
  }

-- | Constructs a 'Continued' object.
continue :: obj
         -- ^ A value.
         -> (obj -> DevOp env a)
         -- ^ A function to build a DevOp.
         -> (obj -> BinaryCall)
         -- ^ A function to build a suitable callback.
         -> Continued env a
continue = Continued

-- | Constructs a 'Continued' from a DevOp with no free variable and its
-- equivalent BinaryCall.
--
-- This function's BinaryCall should respect the same contract as 'continue'.
continueConst :: DevOp env a -> BinaryCall -> Continued env a
continueConst tgt call =
    let obj = ()
        fDevop () = tgt
        fCall () = call
    in continue obj fDevop fCall

-- | Observes the result of a Continued object.
eval :: env -> Continued env a -> Maybe a
eval env (Continued arg f _) = runDevOp env $ f arg

-- | Constructs a 'BinaryCall' that will call-back the Continued object.
callback :: Continued env a -> BinaryCall
callback (Continued arg _ g) = g arg
