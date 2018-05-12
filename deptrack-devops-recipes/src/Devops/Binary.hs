{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Devops.Binary (
    Binary (..) , bin , binary , binaryPath
  , HasBinary
  ) where

import           Data.Proxy   (Proxy (..))
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import           Devops.Base

-- | A Binary tagged with a symbol.
--
-- The type tag can help passing application names in the type system.
data Binary (c :: Symbol) = Binary !FilePath

bin :: FilePath -> Binary c
bin path = Binary path

binaryPath :: Binary c -> FilePath
binaryPath (Binary x) = x

-- A binary which information is passed as a type argument.
binary :: (KnownSymbol c) => DevOp env (Binary c)
binary = pure $ f Proxy
  where f :: (KnownSymbol a) => Proxy a -> Binary a
        f proxy = bin (symbolVal proxy)

class HasBinary a (c :: Symbol) where
