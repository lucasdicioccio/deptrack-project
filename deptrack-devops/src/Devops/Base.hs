{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

-- The base of Devops, using DepTrack.
--
-- Basically like an applicative computation building a set of continuations
-- that run in an interpreter to turn nodes up/down.
module Devops.Base (
    PreOp (..)
  , rawpreop
  , Op (..)
  , OpDescription (..)
  , OpFunctions (..)
  , DevOp , DevOpT
  , runPreOp
  , preopType
  , OpUniqueId , preOpUniqueId
  , OpCheck , CheckResult (..) , fromBool , noCheck
  , OpAction , noAction
  , buildOp
  , buildPreOp
  , noop
  , neutralize
  , TypedPreOp , castPreop
  , devop
  , Name
  --
  , runDevOp
  ) where

import           Control.Monad.Identity (Identity, runIdentity)
import           Data.Hashable          (Hashable (..), hash)
import           Data.Proxy
import qualified Safe
import           Data.Text              (Text)
import           Data.Typeable          (TypeRep, Typeable, cast, typeOf)
import           GHC.Generics           (Generic)

import           DepTrack

type Name = Text

-- | Handy name for tracking DevOp dependencies.
type DevOpT m a = DepTrackT PreOp m a

-- | Handy name for tracking DevOp dependencies using a pure computation
-- (recommended).
type DevOp a = DevOpT [] a

-- | Evaluates the return value of a DevOp, discarding the dependencies.
runDevOp :: DevOp a -> Maybe a
runDevOp = Safe.headMay . value

-- | Encapsulates a deferred `Op` along with an `a` argument to generate it.
--
-- The PreOp is more or less a continuation to produce an Op (which is a set
-- of actions to turnup/turndown system states).
--
-- This definition uses existential quantification with a Typeable constraint:
-- * generally, we do not care about the intermediate type
-- * however, we may want to inspect dependency nodes to apply some tree/graph
-- conversion
-- * we don't want to explicitly require library users to create a gigantic
-- sum-type
data PreOp = forall a. Typeable a => PreOp !a !(a -> Op)

-- | Applies the argument and the function in a PreOp to get an Op.
runPreOp :: PreOp -> Op
runPreOp (PreOp x f) = f x

-- | Almost like a PreOp, but which exposes the type of the intermediary
-- value.
type TypedPreOp a = (a, a -> Op)

-- | Convert a PreOp to a TypedPreOp at runtime.
castPreop :: Typeable a => Proxy a -> PreOp -> Maybe (TypedPreOp a)
castPreop _ (PreOp x f) = (,) <$> cast x <*> cast f

-- | Reads the runtime representation of the PreOp argument.
--
-- This function is useful to display or filter dependency nodes at runtime.
preopType :: PreOp -> TypeRep
preopType (PreOp x _) = typeOf x

-- | The identifier for a PreOp.
preOpUniqueId :: PreOp -> OpUniqueId
preOpUniqueId = opUniqueId . runPreOp

instance Show PreOp where
  show = show . runPreOp

instance Eq PreOp where
  preop1 == preop2 = opDescription (runPreOp preop1) == opDescription (runPreOp preop2)

type OpUniqueId = Int

-- | An actual system-level operation that can be tracked and depended on.
-- `Op`s provide standard `OpFunctions` for actually enacting commands. They are
-- identified by a `OpUniqueId` which is, as it name implies, is guaranteed to be unique
-- across a whole `DepTrack` graph.
data Op = Op { opDescription :: !OpDescription
             , opFunctions   :: !OpFunctions
             , opUniqueId    :: !OpUniqueId
             }

instance Show Op where
  show (Op desc _ _) = "Op (" ++ show desc ++ ", <...functions...>)"

data OpDescription = OpDescription { opName          :: !Name
                                   , opDocumentation :: !Text
                                   } deriving (Show, Eq, Ord, Generic)

instance Hashable OpDescription

type Reason = String -- reason for a failure

data CheckResult =
    Skipped
  -- ^ the Check was skipped (e.g., it's not meaningful or the actions are idempotent and cheap => checking is not useful)
  | Unknown
  -- ^ the Check has not taken place or not succeeded for unknown reasons
  | Success
  -- ^ the Check finished and determined a success
  | Failure !Reason
  -- ^ the Check finished and determined a failure
  deriving (Show, Read, Eq, Ord)

-- | Transforms True into Success, False into a Failure.
fromBool :: Bool -> CheckResult
fromBool (!True) = Success
fromBool (!False) = Failure "false (fromBool)"

type OpCheck = IO CheckResult
type OpAction = IO ()

-- | Functions that can be run on an `Op` object, e.g. a system dependency to enact
-- commands.
data OpFunctions = OpFunctions { opCheck    :: !OpCheck
                               , opTurnup   :: !OpAction
                               , opTurndown :: !OpAction
                               , opReload   :: !OpAction
                               }

noCheck :: OpCheck
noCheck = return Skipped

noAction :: OpAction
noAction = return ()

-- | Projects a Typeable object to a Preop using a projection function.
-- This is a low-level projection function.
rawpreop :: Typeable a => a -> (a -> Op) -> PreOp
rawpreop v f = PreOp v f

-- | Build the internal representation for an 'Op'.
buildOp :: Name -> Text -> OpCheck -> OpAction -> OpAction -> OpAction -> Op
buildOp a b f1 f2 f3 f4 =
  let desc      = (OpDescription a b)     in
  let oid       = hash desc               in
  let functions = OpFunctions f1 f2 f3 f4 in
  Op desc functions oid

-- | Build the internal representation for a 'PreOp'.
buildPreOp :: Name -> Text -> OpCheck -> OpAction -> OpAction -> OpAction -> PreOp
buildPreOp a b f1 f2 f3 f4 = let val = buildOp a b f1 f2 f3 f4
  in rawpreop val id

-- | Simple no-op PreOp.
data NoOp = NoOp deriving (Show,Typeable)

-- | Returns a noop.
noop :: Name -> Text -> PreOp
noop a b = rawpreop NoOp (const $ buildOp a b noCheck noAction noAction noAction)

-- | Takes an Op and makes it a PreOp with same description but with noop
-- checks and actions.
neutralize :: Op -> PreOp
neutralize (Op desc _ oid) =
  let val = Op desc (OpFunctions noCheck noAction noAction noAction) oid
  in rawpreop val id

-- | Tracks dependencies to build an object given a pair of projection --
-- functions and a DepTrackT computation tracking predecessors.
devop :: (Typeable b, Monad m)
  => (a -> b) -> (a -> Op)
  -> DepTrackT PreOp m a -> DepTrackT PreOp m b
devop f g a = fmap f $ track g' a
  where g' v = let !o = g v in rawpreop (f v) (const o)
