{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |Provides a kind of Key/Value store to decouple declaring references and resolving them
module Devops.Ref where

import           Data.Monoid
import           Data.Text
import           Data.Typeable
import           DepTrack
import           Devops.Base

type Key = Text

-- | A textual reference of type `a`
newtype Ref a = Ref { key :: Key }

data Resolver a = Resolver { resolvedKey :: Key
                           , resolver    :: IO a }

instance Show (Resolver a) where
  show = unpack . resolvedKey

-- | The class of `a` things that can be resolved in context `c`
class HasResolver a c where
  resolve :: Key -> c -> IO a

saveRef :: Key -> DevOp (Ref a)
saveRef = track mkOp . return . Ref
  where
    mkOp (Ref k) = noop ("reference: " <> k) ("save reference for later use " <> k)


resolveRef :: (HasResolver a c, Typeable a, Show c) => DevOp (Ref a) -> DevOp c -> DevOp (Resolver a)
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


