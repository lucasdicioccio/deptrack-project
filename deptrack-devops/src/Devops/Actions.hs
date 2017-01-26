{-# LANGUAGE TupleSections #-}

module Devops.Actions (
    concurrentTurnup , concurrentTurndown , concurrentUpkeep , checkStatuses
  , display , dotify , dotifyWithStatuses
  , listUniqNodes
  ) where

import           Control.Concurrent.Async    (Async, async, mapConcurrently,
                                              wait)
import           Control.Concurrent.STM      (STM, TMVar, atomically,
                                              newEmptyTMVarIO, putTMVar,
                                              takeTMVar)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar,
                                              writeTVar)
import           Control.Lens                (view)
import           Control.Monad               (void)
import qualified Data.Array                  as Array
import           Data.Graph                  (Vertex, edges, topSort,
                                              transposeG, vertices)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (catMaybes)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as Text
import           Data.Tree                   (Tree (..), drawForest, flatten)
import           Text.Dot                    (Dot, edge, showDot, userNode,
                                              userNodeId)

import           Devops.Graph
import           Devops.Base                 (CheckResult (..), Op (..),
                                              OpDescription (..),
                                              OpFunctions (..), OpUniqueId,
                                              PreOp, opUniqueId, preOpUniqueId,
                                              runPreOp)

-- | Turns up a graph concurrently.
concurrentTurnup :: OpGraph -> IO ()
concurrentTurnup graph = do
    let s = snapshot TurnedUp graph emptyIntents
    sm <- atomically $ makeStatusesMap s
    asyncTurnupGraph noBroadcast sm s graph

-- | Keeps a graph up concurrently.
concurrentUpkeep :: OpGraph -> IO ()
concurrentUpkeep graph = do
    let s = snapshot TurnedUp graph emptyIntents
    sm <- atomically $ makeStatusesMap s
    upkeepGraph noBroadcast sm s graph defaultUpKeepFSM defaultDownKeepFSM

-- | Turns down a graph concurrently.
concurrentTurndown :: OpGraph -> IO ()
concurrentTurndown (g,f1,f2) = do
    let graph = (transposeG g, f1, f2)
    let s = snapshot TurnedUp graph emptyIntents
    sm <- atomically $ makeStatusesMap s
    asyncTurndownGraph noBroadcast sm s graph

-- | Checks the graph and returns a dot formatted graph.
checkStatuses :: OpGraph -> IO (Map OpUniqueId CheckResult)
checkStatuses graph = do
    let s = snapshot TurnedUp graph emptyIntents
    statuses <- atomically $ makeStatusesMap s
    checkWholeGraph noBroadcast statuses s graph
    atomically $ extractStatuses statuses
  where
    extractStatuses :: OpStatusesMap -> STM (Map OpUniqueId CheckResult)
    extractStatuses statuses =
        Map.fromList <$> traverse extractOne (Map.toList statuses)
    extractOne :: (OpUniqueId, TVar OpStatus) -> STM (OpUniqueId, CheckResult)
    extractOne (opId, tvar) = do
        status <- readTVar tvar
        return $ (opId, view opCheckResult status)

-- | Display a forest of operations.
display :: [Tree PreOp] -> IO ()
display = putStrLn . drawForest . (fmap . fmap) (show . opDescription . runPreOp)

-- | Lists the uniq list of nodes.
listUniqNodes :: [Tree PreOp] -> IO ()
listUniqNodes forest =
  let uniq f xs = Map.toList . Map.fromList $ zip (map f xs) xs
  in putStrLn . unlines . map (\(k,v) -> show (k, opName $ opDescription v)) . uniq opUniqueId . map runPreOp . concatMap flatten $ forest

-- | Builds a dot-formatted representation of the graph
dotify :: OpGraph -> String
dotify = dotifyWith nameAttributes

-- | Same as dotify but also colorize based on statuses passed in second argument.
dotifyWithStatuses :: OpGraph -> Map OpUniqueId CheckResult -> String
dotifyWithStatuses graph x = 
    let allAttributes = nameAttributes <> colorFromStatusAttributes x
    in dotifyWith allAttributes graph

-- | Returns a .dot formatted string of a graph using a projection function to
-- format every PreOp using .dot valid key/value node attributes.
dotifyWith :: (PreOp -> [(String,String)]) -> OpGraph -> String
dotifyWith attributes (g,lookupF,_) =
    showDot dotted
  where
    dotted :: Dot ()
    dotted = do
        let node v = y where (y,_,_) = lookupF v
        let vs = vertices g
        let es = filter (uncurry (/=)) $ edges g
        mapM_ (\i -> userNode (userNodeId i) (attributes (node i))) vs
        mapM_ (\(i,j) -> edge (userNodeId i) (userNodeId j) []) es

nameAttributes :: PreOp -> [(String, String)]
nameAttributes preOp =
  let o = runPreOp preOp in
  [("label", Text.unpack $ opName $ opDescription o)]

colorFromStatusAttributes :: Map OpUniqueId CheckResult -> PreOp -> [(String, String)]
colorFromStatusAttributes c op =
    let status = Map.lookup (preOpUniqueId op) c
    in maybe unknownStatusLabels labelsFromStatus status
  where
    labelsFromStatus :: CheckResult -> [(String, String)]
    labelsFromStatus Success = [("color", "green")]
    labelsFromStatus Skipped = [("color", "yellow")]
    labelsFromStatus Unknown = [("color", "blue")]
    labelsFromStatus _       = [("color", "red")]
    unknownStatusLabels :: [(String, String)]
    unknownStatusLabels = [("shape", "egg")]

