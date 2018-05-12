{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE RankNTypes            #-}
-- |Provides a kind of Key/Value store to decouple declaring references and resolving them
module Devops.Ref where

import           Data.Monoid
import           Data.Text
import           Data.Typeable
import           Devops.Base

type Key = Text

-- | A textual reference of type `a`
newtype Ref a = Ref { key :: Key }

data Resolver a =
    Resolver { resolvedKey :: Key
             , resolver    :: IO a
             } deriving Functor

instance Show (Resolver a) where
  show = unpack . resolvedKey

type Evaluator env b = forall a. DevOp env a -> b

delay
  :: DevOp env (Resolver a)
  -> (a -> DevOp env b)
  -> DevOp env (Resolver (DevOp env b))
delay r f = (fmap . fmap) f r

delayedEval
  :: (Monad m, Typeable a)
  => DevOpT e m (Resolver (DevOp env a))
  -> (DevOp env a -> OpFunctions)
  -> env
  -> DevOpT e m (Resolver (Maybe a))
delayedEval mkR eval env = devop fst mkOp $ do
    r <- mkR
    let devopIO = resolver r
    return (Resolver (resolvedKey r) (fmap (runDevOp env) devopIO), devopIO)
  where
    mkOp (Resolver k _, devopIO) = buildOp
                ("delayed " <> k)
                "delayedEval a resolved op"
                (opCheck . eval =<< devopIO)
                (opTurnup . eval =<< devopIO)
                (opTurndown . eval =<< devopIO)
                (opReload . eval =<< devopIO)

-- | The class of `a` things that can be resolved in context `c`
class HasResolver a c where
  resolve :: Key -> c -> IO a

saveRef
  :: Key
  -> DevOp env (Ref a)
saveRef =
    track mkOp . return . Ref
  where
    mkOp (Ref k) = noop ("reference: " <> k) ("save reference for later use " <> k)


resolveRef
  :: (HasResolver a c, Typeable a)
  => DevOp env (Ref a)
  -> DevOp env c
  -> DevOp env (Resolver a)
resolveRef mkRef context = devop snd mkOp $ do
  Ref k <- mkRef
  ctx <- context
  return (k, Resolver k $ resolve k ctx)
    where mkOp (k, _) = buildOp
            ("resolve  " <> k)
            "a resolver for some key in some context"
            (pure Success)
            noAction
            noAction
            noAction
