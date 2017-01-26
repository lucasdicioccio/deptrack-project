{-# LANGUAGE DeriveFunctor #-}

module DepTrack.DepCrumb (
    DepCrumb (..)
  ) where

data DepCrumb a
  = Push | Pop a       -- push a level, record the poped value
  | SpadeIn | SpadeMiddle | SpadeOut -- wraps a node
  deriving (Functor, Show)
