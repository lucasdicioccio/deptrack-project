{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

module DepTrack (
    DepTrackT
  , track
  , declare
  , ToDep (..)
  , dep
  , evalDeps
  , value
  , evalDepForest
  , evalDepGraph
  , evalDepForest1
  , GraphData
  , evalDepGraph1
  , buildGraph
  , inject
  ) where

import           Control.Applicative         (liftA)
import           Control.Monad.Writer.Strict (WriterT, writer, tell, runWriterT)
import           Data.DList                  (DList)
import qualified Data.DList                  as DList
import           Data.Graph                  (Graph, Vertex)
import qualified Data.Graph                  as Graph
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Monoid                 ((<>))
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Tree                   (Forest, Tree (..))
import qualified Data.Tree                   as Tree
import qualified Text.Parsec                 as Parsec

import           DepTrack.DepCrumb
import           DepTrack.Parsing

type DepTrackT a m = WriterT (DList (DepCrumb a)) m

-- | Declare the dependencies of a given computation.
declare :: (Monad m) => a -> DepTrackT a m b -> DepTrackT a m b
declare obj = track (const obj)

-- | Nests a computation within annotation to record the result of the
-- computation as a dependencies.
--
-- The computation to annotate is the second parameter.
track :: (Monad m) => (b -> a) -> DepTrackT a m b -> DepTrackT a m b
track f op = do
    push
    a <- op
    _ <- pop (f a)
    return a
  where
    push :: (Monad m) => DepTrackT a m ()
    push = tell (DList.singleton Push)

    pop :: (Monad m) => a -> DepTrackT a m a
    pop a = writer (a, DList.singleton (Pop a))

-- | Artificially injects dependencies from a computation to the
-- dependencies of another given computation.
--
-- Beware that one can easily create dependency cycles when using inject.
-- This function is however necessary when you have no control over some
-- library code and you want to artificially wrap it with new dependencies.
inject :: (Monad m) => DepTrackT a m b -> DepTrackT a m c -> DepTrackT a m (b, c)
inject m1 m2 = do
    spadeIn
    a <- m1
    spadeMiddle
    b <- m2
    spadeOut
    return (a,b)
  where
    spadeIn :: (Monad m) => DepTrackT a m ()
    spadeIn = tell (DList.singleton SpadeIn)
    spadeMiddle :: (Monad m) => DepTrackT a m ()
    spadeMiddle = tell (DList.singleton SpadeMiddle)
    spadeOut :: (Monad m) => DepTrackT a m ()
    spadeOut = tell (DList.singleton SpadeOut)

forestToPairs :: Forest a -> [(a,[a])]
forestToPairs = concatMap treeToPairs

treeToPairs :: Tree a -> [(a,[a])]
treeToPairs (Node x ts) = directDeps : childrenDeps
  where
    directDeps = (x, fmap Tree.rootLabel ts)
    childrenDeps = forestToPairs ts

mapGraphFromForest :: Ord k => (a -> k) -> Forest a -> Map k (a, Set k)
mapGraphFromForest fKey ts = Map.fromListWithKey f (formatPairs ts)
  where
    f _ (a,xs) (_,ys) = (a, xs <> ys)
    formatPairs = fmap (\(x,ys) -> (fKey x, (x, Set.fromList (fmap fKey ys)))) . forestToPairs

graphFromMap :: Ord k => Map k (a, Set k) -> GraphData a k
graphFromMap = Graph.graphFromEdges . fmap formatPair . Map.toList
  where
    formatPair (k, (a, ks)) = (a, k, Set.toList ks)

-- | Builds a Graph with auxilliary from nodes in a Forest.
--
-- The identity of a node in the forest is given using a key-projection
-- function.
buildGraph :: Ord k => (a -> k) -> Forest a -> GraphData a k
buildGraph f ts = graphFromMap $ mapGraphFromForest f ts

-- | Evaluates a computation, dropping the trace of dependencies.
value :: (Monad m) => DepTrackT a m b -> m b
value = fmap fst . runWriterT

-- | Evaluates a computation, tracking dependencies as a sequence of
-- dependency-tracking actions (crumbs).
evalDeps :: DepTrackT a m b -> m (b, DList (DepCrumb a))
evalDeps = runWriterT

-- | Evaluates a computation, tracking dependencies as a forest.
evalDepForest :: (Monad m, Show a) =>
  DepTrackT a m b -> m (b, Either Parsec.ParseError (Forest a))
evalDepForest = (fmap . fmap) f . evalDeps
  where
    f crumbs = let crumbs' = DList.toList crumbs
               in Parsec.parse dependencies "" crumbs'

-- | Evaluates a computation, tracking dependencies as a forest.
evalDepForest1 :: (Monad m, Show a) =>
  DepTrackT a m b -> m (b, Forest a)
evalDepForest1 = liftA (fmap (fromRight "deptrack is broken")) . evalDepForest

fromRight :: String -> Either a b -> b
fromRight _ (Right x) = x
fromRight msg (Left _)  = error msg

-- | General type for graphs of computations
type GraphData a k = ( Graph
                       -- The underlying abtract directed `Graph` structure
                     , Vertex -> (a, k, [k])
                       -- For each `Vertex` in the graph, provides associated data `a`, key `k` and
                       -- a list of keys to linked vertices
                     , k -> Maybe Vertex
                       -- Retrieves actual `Vertex` from its key
                     )

-- | Evaluates a computation, tracking dependencies as a graph with an
-- auxilliary function.
--
-- Node identity is determined using a projection function.
evalDepGraph :: (Monad m, Ord k, Show a) =>
                DepTrackT a m b  -- ^ The dependency tracked computation
             -> (a -> k)         -- ^ How to get a key from a node's data
             -> m (b, Either Parsec.ParseError (GraphData a k))
evalDepGraph x fKey = (fmap . fmap . fmap) (buildGraph fKey) (evalDepForest x)

evalDepGraph1 :: (Monad m, Ord k, Show a) =>
  DepTrackT a m b -> (a -> k) -> m (b, GraphData a k)
evalDepGraph1 x fKey = liftA (fmap (fromRight "deptrack is broken")) $ evalDepGraph x fKey

-- | Convenience typeclass to decouple the definition of projection functions
-- to dependency nodes.
--
-- This typeclass allows to write dep-recording code once and specialize the
-- type of node by only switching the parameter.
class ToDep a b where
   toDep :: a -> b

-- | Like track but using the projection function resolved from the ToDep
-- typeclass.
dep :: (ToDep a b, Monad m) =>
  DepTrackT b m a -> DepTrackT b m a
dep = track toDep
