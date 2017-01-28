
module Devops.Optimize (
    optimizeDebianPackages
  , applyTransformation
  , noOpNodes
  , collectTypedPreOps
  ) where

import           Control.Lens  (view, _1, _2)
import           Data.Proxy
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.Tree
import           Data.Typeable (Typeable)

import           Devops.Debian (DebianPackagesSet (..))
import           Devops.Base


-- | Replace a set of ops identified by their unique id
-- with placeholder node having the same description
noOpNodes :: Set OpUniqueId -> Forest PreOp -> Forest PreOp
noOpNodes hashes xs =
  let predicate n = Set.member (preOpUniqueId n) hashes
      transform = neutralize . runPreOp
  in applyTransformation predicate transform xs

-- | Apply a transformation to nodes matching a predicate.
applyTransformation :: (PreOp -> Bool) -> (PreOp -> PreOp) -> Forest PreOp -> Forest PreOp
applyTransformation predicate transform xs =
  fmap go xs
  where go :: Tree PreOp -> Tree PreOp
        go (Node n ys)
            | predicate n = (Node (transform n) (applyTransformation predicate transform ys))
            | otherwise = (Node n (applyTransformation predicate transform ys))

-- | Collects a list of TypedPreOp for which the dynamic type corresponds to --
-- one requested with a Proxy.
-- Duplicates may occur because this function works on a Forest.
collectTypedPreOps :: Typeable a
  => Proxy a -> Forest PreOp -> [TypedPreOp a]
collectTypedPreOps proxy xs = concatMap (go proxy) xs
  where go :: Typeable a => Proxy a -> Tree PreOp -> [TypedPreOp a]
        go proxy (Node p children) =
              let pkgs = castPreop proxy p
                  zs = collectTypedPreOps proxy children
              in case pkgs of
                    Nothing      -> zs
                    (Just (k,f)) -> (k,f):zs


-- | Collects all debian packages and run them in a single pass.
optimizeDebianPackages :: Forest PreOp -> Forest PreOp
optimizeDebianPackages xs =
  let ms = collectTypedPreOps (Proxy :: Proxy DebianPackagesSet) xs
      merged = mconcat $ map (view _1) ms
      proj = view _2 . head $ ms -- we take the projection function
  in if null ms
     then xs
     else replaceDebianPackagesOpsWith (rawpreop merged proj) xs

replaceDebianPackagesOpsWith :: PreOp -> Forest PreOp -> Forest PreOp
replaceDebianPackagesOpsWith rep xs = fmap go xs
  where go :: Tree PreOp -> Tree PreOp
        go (Node p children) =
              let pkgs = castPreop (Proxy :: Proxy DebianPackagesSet) p
                  zs = replaceDebianPackagesOpsWith rep children
              in case pkgs of
                    Nothing                        -> (Node p zs)
                    (Just (DebianPackagesSet _,_)) -> (Node rep zs)
