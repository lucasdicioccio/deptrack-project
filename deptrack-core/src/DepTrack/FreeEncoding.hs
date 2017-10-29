{-# LANGUAGE GADTs #-}
-- | Another -- experimental -- encoding for DepTrack computations.
--
-- See 'DT' constructors.
module DepTrack.FreeEncoding (DT(..), dep, decl, atLeast) where

import Data.Bifunctor
import Control.Applicative (Alternative(..), optional)

data DT d a where
  Return :: Maybe a -> DT d a
  -- ^ free return with potential failure
  -- The failure is required for defining an 'empty' Alternative instance.
  Ap     :: DT d (x -> b) -> DT d x -> DT d b
  -- ^ free ap
  Bind   :: DT d a -> (a -> DT d b) -> DT d b
  -- ^ free bind
  Alt    :: DT d a -> DT d a -> DT d a
  -- ^ free <|>
  NAmong :: Int -> [DT d a] -> ([a] -> b) -> DT d b
  -- ^ Encodes that N unspecified dependencies among M are required (e.g., for
  -- encoding majorities). This is a better encoding than generating all
  -- combinatorial alternatives. We need to carry a CoYoneda style function
  -- for the fmap instance.
  Dep    :: (a -> d) -> DT d a -> (a -> DT d b) -> DT d b
  -- ^ Binds computations while recording the specific dependency.

rmap :: (a -> b) -> DT d a -> DT d b
rmap f (Return v)        = Return (fmap f v)
rmap f (Ap g x)          = Ap (rmap (f.) g) x
rmap f (Bind x g)        = Bind x (\v -> rmap f $ g v)
rmap f (Alt l r)         = Alt (rmap f l) (rmap f r)
rmap f (NAmong n xs g)   = NAmong n xs (f . g)
rmap f (Dep p x g)       = Dep p x (\v -> rmap f $ g v)

lmap :: (d -> e) -> DT d a -> DT e a
lmap f (Dep p x g)       = Dep (f . p) (lmap f x) (\v -> lmap f $ g v)
lmap f (Return v)        = Return v
lmap f (Ap g x)          = Ap (lmap f g) (lmap f x)
lmap f (Bind x g)        = Bind (lmap f x) (\v -> lmap f $ g v)
lmap f (Alt l r)         = Alt (lmap f l) (lmap f r)
lmap f (NAmong n xs g)   = NAmong n ((fmap (lmap f)) xs) g

instance Functor (DT d) where
  fmap = rmap

instance Bifunctor DT where
  bimap f g = lmap f . rmap g

instance Applicative (DT d) where
  pure  = Return . Just
  (<*>) = Ap

instance Monad (DT d) where
  return  = pure
  (>>=)   = Bind

instance Alternative (DT d) where
  empty = Return Nothing
  (<|>) = Alt
  many dt = do
    m <- optional dt
    case m of
      Nothing -> return []
      Just x  -> (x:) <$> many dt
  some dt = Bind dt (\x -> (x:) <$> many dt)

dep :: (a -> d) -> DT d a -> DT d a
dep f act = Dep f act pure

decl :: d -> DT d a -> DT d a
decl v = dep (const v)

atLeast :: Int -> [DT d a] -> DT d [a]
atLeast n xs = NAmong n xs id
