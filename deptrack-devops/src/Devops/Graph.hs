{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Devops.Graph (
    WantedDirection (..)
  , Stability (..)
  , OpGraph
  , OpStatus , opCheckResult
  , OpStatusesMap
  , OpIntent , intentPreOp , intentDirection
  , Intents , emptyIntents
  --
  , Broadcast
  , noBroadcast
  -- * Asynchronous Operations
  , snapshot , makeStatusesMap
  , asyncTurnupGraph , asyncTurndownGraph , checkWholeGraph , upkeepGraph
  , defaultUpKeepFSM , defaultDownKeepFSM
  --
  , waitStability
  -- * Synchronous Operations
  , syncTurnupGraph, syncTurnDownGraph
  ) where

import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.Async    (Async, async, mapConcurrently)
import           Control.Concurrent.STM      (STM, atomically, retry)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVar,
                                              readTVar, readTVarIO, writeTVar)
import           Control.Lens                (set, view)
import           Control.Lens.TH             (makeLenses)
import           Control.Monad               (mapM_, void)
import qualified Data.Array                  as Array
import qualified Data.Graph                  as Graph
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))
import qualified Data.Text                   as Text
import           GHC.Generics

import           DepTrack                    (GraphData)
import           Devops.Base                 (CheckResult (..), Op (..),
                                              OpDescription (..),
                                              OpFunctions (..), OpUniqueId,
                                              PreOp, preOpUniqueId, runPreOp)

-- | The intended direction of an operation.
data WantedDirection = TurnedUp | TurnedDown
  deriving (Show,Eq,Ord,Generic)

-- | NIH Stream representing an infinite non-empty lists.
-- TODO:
-- * carry some logical version/timestamp
-- * take some generally-accepted Stream implementation.
data Stream a = Cons a (Stream a)
  deriving (Functor)
instance Applicative Stream where
    pure x = let stream = Cons x stream in stream
    Cons f fs <*> Cons a as = Cons (f a) (fs <*> as)

-- | A snapshot representing the intent for a single operation at a given time.
data OpIntent =
    OpIntent { _intentDirection :: WantedDirection
             , _intentPreOp     :: PreOp
             }
makeLenses ''OpIntent

-- | The stability an operation reached.
data Stability = Stable | Transient
  deriving (Show,Eq,Ord,Generic)

-- | The observed status for an operation.
data OpStatus =
    OpStatus { _opCheckResult :: !CheckResult
             , _opDirection   :: !WantedDirection
             , _opStability   :: !Stability
             } deriving Show
makeLenses ''OpStatus

-- | A simple handler to be used when turning-up/down a graph sequentially
type SyncOpHandler a = PreOp -> IO a

-- | A `PreOp` handler when turning up/down a graph concurrently
type AsyncOpHandler a = PreOp
                -- ^ the operation
                -> TVar OpStatus
                -- ^ the operation status
                -> [TVar OpStatus]
                -- ^ children operations statuses
                -> [TVar OpStatus]
                -- ^ parents operations statuses
                -> OpHistory
                -- ^ historical intents for this operation
                -> IO a

-- | An OpGraph is a Graph containing PreOp.
type OpGraph = GraphData PreOp OpUniqueId

-- | A succession of OpIntent to model intentions varying with events.
type OpHistory = Stream OpIntent

-- | A map representing the collection of histories for a set of operations.
type Intents = Map OpUniqueId OpHistory
emptyIntents :: Intents
emptyIntents = Map.empty

-- | A map representing the mutable status for a set of operations.
type OpStatusesMap = Map OpUniqueId (TVar OpStatus)

-- | Concurrently spawn an action for each item in an OpGraph.
traverseOpGraph ::
     OpStatusesMap
  -> Intents
  -> OpGraph
  -> AsyncOpHandler a
  -> IO [a]
traverseOpGraph statusMap intents (g,lookupVertex,_) handler = do
    mapConcurrently (traverseOne handler) (Graph.vertices g)
  where
    traverseOne :: AsyncOpHandler a -> Graph.Vertex -> IO a
    traverseOne go vertex = do
        let (preop,oid,_) = lookupVertex vertex
        let childrenOids = fmap (\(_,x,_) -> x) $ fmap lookupVertex (g Array.! vertex)
        let parentsOids   = fmap (\(_,x,_) -> x) $ fmap lookupVertex (Graph.transposeG g Array.! vertex)
        let tvar = Map.lookup oid statusMap
        let ophistory = Map.lookup oid intents
        let childrenTVars = traverse (flip Map.lookup statusMap)
                                     (childrenOids)
        let parentsTVars = traverse (flip Map.lookup statusMap)
                                     (parentsOids)
        let action = go preop <$> tvar <*> childrenTVars <*> parentsTVars <*> ophistory
        fromMaybe (error "tvar/ophistory mismatch") action

-- | Atomically wait until all statuses are stable after reaching a given
-- direction.
waitStability :: WantedDirection -> Stability -> [TVar OpStatus] -> STM ()
waitStability dir stab statusTVars = do
    statuses <- traverse readTVar statusTVars
    let stable x = stab == view opStability x
    let rightDirection x = dir == view opDirection x
    let ok x = stable x && rightDirection x
    let ready = all ok statuses
    if ready then return () else retry

type Broadcast = (OpUniqueId,CheckResult,Stability,WantedDirection) -> IO ()
noBroadcast :: Broadcast
noBroadcast = const (return ())

-- | Turn-up a graph sequentially
--
-- Topologically sorts the graph then invokes `TurnUp` action synchronously
-- through each node. Note the order of execution is deterministic.
syncTurnupGraph :: Broadcast -> OpGraph -> IO ()
syncTurnupGraph bcast (graph,lookupVertex,_) =
    mapM_ go (Graph.topSort graph)
    where
      go :: Graph.Vertex -> IO ()
      go vertex = do
          let (preop,oid,_) = lookupVertex vertex
              desc          = opName . opDescription $ runPreOp preop
              funs          = opFunctions $ runPreOp preop
              turnup        = opTurnup funs
              reload        = opReload funs
              check         = opCheck  funs
          bcast (oid,Unknown,Transient,TurnedUp)
          print ("pre-checking: " <> desc)
          currentStatus <- check
          case currentStatus of
              Success -> print ("reloading: " <> desc) >> reload
              _       -> print ("turning-up: " <> desc) >> turnup
          print ("turnup-done: " <> desc)
          bcast (oid,Success,Stable,TurnedUp)

-- | Turn-down a graph sequentially
--
-- Topologically sorts the graph then invokes `TurnDown` action synchronously
-- through each node. Note the order of execution is deterministic.
syncTurnDownGraph :: Broadcast -> OpGraph -> IO ()
syncTurnDownGraph bcast (graph,lookupVertex,_) =
    mapM_ go (Graph.topSort graph)
    where
      go :: Graph.Vertex -> IO ()
      go vertex = do
          let (preop,oid,_) = lookupVertex vertex
              desc          = opName . opDescription $ runPreOp preop
              turndown      = opTurndown $ opFunctions $ runPreOp preop
          bcast (oid,Unknown,Transient,TurnedDown)
          print ("turning-down: "<> desc)
          turndown
          print ("turndown-done: " <> desc)
          bcast (oid,Success,Stable,TurnedDown)

-- forks a node that will turnup a given Op once its children
-- all are green
-- dies early if the graph changes and removes the vertex from OpGraphStatuses
asyncTurnupGraph :: Broadcast
                 -> OpStatusesMap
                 -> Intents
                 -> OpGraph
                 -> IO ()
asyncTurnupGraph bcast statusMap intents graph = do
    void $ traverseOpGraph statusMap intents graph go
  where
    go :: AsyncOpHandler ()
    go preop tvar childrenTVars _ _ = do
        let oid = preOpUniqueId preop
        let desc = opName . opDescription $ runPreOp preop
        let turnup = opTurnup $ opFunctions $ runPreOp preop
        let reload = opReload $ opFunctions $ runPreOp preop
        let check = opCheck $ opFunctions $ runPreOp preop
        print $ "turnup waiting " <> (Text.pack (show (length childrenTVars))) <> " children: " <> desc
        atomically $ do
            waitStability TurnedUp Stable childrenTVars
            modifyTVar' tvar (set opStability Transient)
        bcast (oid,Unknown,Transient,TurnedUp)
        print ("pre-checking: " <> desc)
        currentStatus <- check
        threadDelay 1000000
        case currentStatus of
            Success -> print ("reloading: " <> desc) >> reload
            _       -> print ("turning-up: " <> desc) >> turnup
        atomically $ do
            modifyTVar' tvar (set opStability Stable)
        print ("turnup-done: " <> desc)
        bcast (oid,Success,Stable,TurnedUp)

-- | TODO: no longer transpose graph at the calling sites
asyncTurndownGraph :: Broadcast -> OpStatusesMap -> Intents -> OpGraph -> IO ()
asyncTurndownGraph bcast statusMap intents graph = do
    void $ traverseOpGraph statusMap intents graph go
  where
    -- wait for children to be OK and continue
    go :: AsyncOpHandler ()
    go preop tvar childrenTVars _ _ = do
        let oid = preOpUniqueId preop
        let desc = opName . opDescription $ runPreOp preop
        let turndown = opTurndown $ opFunctions $ runPreOp preop
        print $ "turndown waiting " <> (Text.pack $ show (length childrenTVars)) <> " children: " <> desc
        atomically $ do
            waitStability TurnedDown Stable childrenTVars
            modifyTVar' tvar (set opStability Transient)
        bcast (oid,Unknown,Transient,TurnedDown)
        print ("turning-down: "<> desc)
        turndown
        threadDelay 1000000
        atomically $ do
            modifyTVar' tvar (set opStability Stable)
        print ("turndown-done: " <> desc)
        bcast (oid,Success,Stable,TurnedDown)

checkWholeGraph :: Broadcast
                -> OpStatusesMap
                -> Intents
                -> OpGraph
                -> IO [Async (OpUniqueId, CheckResult, Stability, WantedDirection)]
checkWholeGraph bcast statusMap intents graph = do
    traverseOpGraph statusMap intents graph go'
  where
    go' :: AsyncOpHandler (Async (OpUniqueId, CheckResult, Stability, WantedDirection))
    go' preop tvar _ _ _ = go (runPreOp preop) tvar

    go :: Op -> (TVar OpStatus) -> IO (Async (OpUniqueId, CheckResult, Stability, WantedDirection))
    go op tvar = do
        async $ do
            !newCheckResult <- opCheck $ opFunctions $ op
            (_,new) <- atomically $ do
                oldStatus <- readTVar tvar
                let !newStatus = set opCheckResult newCheckResult oldStatus
                writeTVar tvar newStatus
                return (oldStatus, newStatus)
            let oid = opUniqueId op
            let ret = (oid,(view opCheckResult new),
                       (view opStability new),
                       (view opDirection new))
            bcast ret -- optimization to be here, could batch at the end
            return ret

data UpkeepState =
    WaitUp
  -- ^ waiting for a trigger to turn this node up
  | Upping
  -- ^ last we know, the node is currently turning up
  | Up
  -- ^ the node is considered turned up

data UpkeepFSMFunctions = UpkeepFSMFunctions {
    _waitUpAndStable :: IO ()
  , _turnupOrReload  :: IO ()
  , _checkUpStatus   :: IO OpStatus
  }
makeLenses ''UpkeepFSMFunctions

data DownkeepFSMFunctions = DownkeepFSMFunctions {
    _waitDownAndStable :: IO ()
  , _performTurndown   :: IO ()
  , _checkDownStatus   :: IO OpStatus
  }
makeLenses ''DownkeepFSMFunctions

data DownkeepState =
    WaitDown
  -- ^ waiting for a trigger to turn this node down
  | Downing
  -- ^ last we know, the node is currently turning down
  | Down
  -- ^ the node is considered turned down

-- | Transpose a intents from a "map of historical intentions" to a "history of
-- maps of intentions".
snapshots :: Intents -> Stream (Map OpUniqueId OpIntent)
snapshots = Map.traverseWithKey (\_ h -> h)

-- | Turns a graph up and keep it up.
upkeepGraph :: Broadcast
            -- ^ function delivering node change updates
            -> OpStatusesMap
            -- ^ instantaneous statuses
            -> Intents
            -- ^ historical intents
            -> OpGraph
            -- ^ graph containing adjacencies
            -> UpkeepFSM
            -- ^ FSM to apply to nodes that must be up
            -> DownkeepFSM
            -- ^ FSM to apply to nodes that must be down
            -> IO ()
upkeepGraph bcast statusMap intents graph upKeepFSM downKeepFSM = do
    void $ traverseOpGraph statusMap intents graph go
  where
    go :: PreOp -> TVar OpStatus -> [TVar OpStatus] -> [TVar OpStatus] -> OpHistory -> IO ()
    go preop tvar childrenTVars parentTVars ophistory = do
        let (Cons intent _) = ophistory
        let !direction = view intentDirection intent
        let !op = runPreOp preop
        let !oid = opUniqueId op
        let !desc = opName $ opDescription op
        let !turnup = opTurnup $ opFunctions op
        let !turndown = opTurndown $ opFunctions op
        let !reload = opReload $ opFunctions op
        let !check = opCheck $ opFunctions op

        let waitUpAndStableFunction = do
                print ("waiting-start: " <> desc)
                atomically $ do
                    waitStability TurnedUp Stable childrenTVars
                    modifyTVar' tvar (set opStability Transient)
                bcast (oid,Unknown,Transient,TurnedUp)

        let waitDownAndStableFunction = do
                print ("waiting-stop: " <> desc)
                atomically $ do
                    waitStability TurnedDown Stable parentTVars
                    modifyTVar' tvar (set opStability Transient)
                bcast (oid,Unknown,Transient,TurnedDown)

        let turnupOrReloadFunction = do
                !status <- readTVarIO tvar
                lastCheck <- case view opCheckResult status of
                    Unknown -> print ("pre-checking: " <> desc) >> check
                    x       -> print ("re-checked: " <> desc) >> return x
                case lastCheck of
                    Success -> print ("reloading: " <> desc) >> reload
                    _       -> print ("turning-up: " <> desc) >> turnup
                atomically $ do
                    modifyTVar' tvar (set opStability Stable)
                bcast (oid,Success,Stable,TurnedUp)

        let performTurndownFunction = do
                print ("turning-down: "<> desc)
                turndown
                atomically $ do
                    modifyTVar' tvar (set opStability Stable)
                bcast (oid,Success,Stable,TurnedDown)

        let checkStatusFunction = do
                print ("checking: " <> desc)
                !newCheckResult <- check
                (_,new) <- atomically $
                    adjustTVar' tvar (set opCheckResult newCheckResult)
                bcast (oid,(view opCheckResult new),
                           (view opStability new),
                           (view opDirection new))
                return new

        let upFunctions = UpkeepFSMFunctions
                waitUpAndStableFunction
                turnupOrReloadFunction
                checkStatusFunction
        let downFunctions = DownkeepFSMFunctions
                waitDownAndStableFunction
                performTurndownFunction
                checkStatusFunction
        case direction of
                 TurnedUp   -> upKeepFSM WaitUp upFunctions
                 TurnedDown -> downKeepFSM WaitDown downFunctions

type UpkeepFSM = UpkeepState -> UpkeepFSMFunctions -> IO ()
type DownkeepFSM = DownkeepState -> DownkeepFSMFunctions -> IO ()

defaultUpKeepFSM :: UpkeepFSM
defaultUpKeepFSM = fsm 100000
  where
    increaseDelay :: Int -> Int
    increaseDelay delay = min (delay * 2) 60000000
    decreaseDelay :: Int -> Int
    decreaseDelay delay = max (floor $ (fromIntegral delay / 2 :: Float) ) 500000

    fsm delay WaitUp xyz = threadDelay delay >> view waitUpAndStable xyz >> fsm delay Upping xyz
    fsm delay Upping xyz = threadDelay delay >> view turnupOrReload xyz >> fsm delay Up xyz
    fsm delay Up xyz     = threadDelay delay >> do
      newStatus <- view checkUpStatus xyz
      case (view opCheckResult newStatus) of
        Success -> fsm (increaseDelay delay) Up xyz
        Skipped -> fsm (increaseDelay delay) Up xyz
        _       -> fsm (decreaseDelay delay) Upping xyz

defaultDownKeepFSM :: DownkeepFSM
defaultDownKeepFSM = fsm
  where
    fsm WaitDown xyz = threadDelay 1000000 >> view waitDownAndStable xyz >> fsm Downing xyz
    fsm Downing xyz  = threadDelay 1000000 >> view performTurndown xyz >> fsm Down xyz
    fsm Down xyz     = threadDelay 3000000 >> do
      newStatus <- view checkDownStatus xyz
      case (view opCheckResult newStatus) of
        (Failure _) -> fsm Down xyz
        _           -> fsm Downing xyz

-- | Computes a snapshot of an OpGraph.
snapshot :: WantedDirection -> OpGraph -> Intents -> Intents
snapshot d (g,f,_) hist =
    let nodes = fmap f (Graph.vertices g) -- graph nodes
        mkPair (p,i,_) = (i, (OpIntent d p)) -- new OpIntent from a node in the OpGraph
        s = Map.fromList (fmap mkPair nodes)   -- new OpIntents to merge
        t _ snap stream = Just $ Cons snap stream -- Stream concatenation on values
    in Map.mergeWithKey t (fmap pure) id s hist -- merge the new snapshot (generalized update)

-- | Returns an operation to atomically build a status map from a graph
-- snapshot.
makeStatusesMap :: Intents -> STM OpStatusesMap
makeStatusesMap intents =
    let (Cons snap _) = snapshots intents
    in Map.fromList <$> traverse mkPair (Map.toList snap)
    where
      mkPair :: (OpUniqueId, OpIntent) -> STM (OpUniqueId, TVar OpStatus)
      mkPair (oid, intent) = do
          let dir = view intentDirection intent
          tvar <- newTVar (newOpStatus dir)
          return (oid,tvar)
      newOpStatus :: WantedDirection -> OpStatus
      newOpStatus dir = OpStatus Unknown dir Transient

adjustTVar' :: TVar a -> (a -> a) -> STM (a, a)
adjustTVar' t f = do
    x <- readTVar t
    let !y = f x
    writeTVar t y
    return (x, y)

