-- | Module providing helpers to get a Dot representation of tracked
-- dependencies during a DepTrack computation.
--
-- This module currently uses 'Text.Dot' from 'dotgen' as a Dot generator, this
-- choice may change in the future.
module DepTrack.Dot (DotDescription(..), dotifyGraphWith, dotify) where

import DepTrack (DepTrackT, GraphData, buildGraph, evalDepForest1)
import Data.Graph (edges, vertices)
import qualified Text.Dot as Dot

-- | A type to hide a Dot program.
newtype DotDescription = DotDescription { getDotDescription :: String }
  deriving (Eq, Ord, Show)

-- | Dotify some pre-calculed GraphData.
dotifyGraphWith
  :: (x -> [(String,String)])
  -- ^ Function to return a set of graphviz key-value attributes (e.g., ("shape","egg"))
  -> GraphData x k
  -- ^ The graph data to represent.
  -> DotDescription
dotifyGraphWith attributes (g,lookupF,_) =
    DotDescription $ Dot.showDot dotted
  where
    dotted :: Dot.Dot ()
    dotted = do
        let node v = y where (y,_,_) = lookupF v
        let vs = vertices g
        let es = filter (uncurry (/=)) $ edges g
        mapM_ (\i -> Dot.userNode (Dot.userNodeId i) (attributes (node i))) vs
        mapM_ (\(i,j) -> Dot.edge (Dot.userNodeId i) (Dot.userNodeId j) []) es

-- | Graphs the dependenciees of a DepTrack computation.
--
-- Throws away the result and only keep the DotDescription.
dotify
  :: (Monad m, Ord k, Show x)
  => (x -> [(String,String)])
  -- ^ Function to return a set of graphviz key-value attributes (e.g., ("shape","egg"))
  -> (x -> k)
  -- ^ Function to identify every node in the graph uniquely. If this function
  -- is non injective you may confuse two distinct nodes as a same node.
  -> DepTrackT x m a
  -- ^ The computation to graph.
  -> m DotDescription
dotify labelsF keyF x =
    dotifyGraphWith labelsF . buildGraph keyF . snd <$> evalDepForest1 x
