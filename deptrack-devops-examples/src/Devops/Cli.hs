
-- | Building-block methods to build a command-line tool able to inspect and
-- turnup/turndown DevOps.
module Devops.Cli (
    Method (..)
  , applyMethod
  , getDependenciesOnly
  , opFromClosureB64
  , graphize
  ) where

import           Control.Distributed.Closure (unclosure)
import           Control.Monad.Identity      (runIdentity)
import qualified Data.Binary                 as Binary
import qualified Data.ByteString.Base64.Lazy as B64
import           Data.ByteString.Lazy        (ByteString)
import           Data.Tree                   (Forest)
import           DepTrack                    (GraphData, buildGraph,
                                              evalDepForest1)
import           Prelude                     hiding (readFile)

import           Devops.Actions              (concurrentTurndown,
                                              concurrentTurnup, concurrentUpkeep, display, defaultDotify,
                                              dotifyWithStatuses,
                                              listUniqNodes, checkStatuses)
import           Devops.Base                 (DevOp, Op (..), OpFunctions (..),
                                              OpUniqueId, PreOp, preOpUniqueId)

data Method =
    TurnUp
  | TurnDown
  | Upkeep
  | Print
  | Dot
  | CheckDot
  | List

getDependenciesOnly :: DevOp a -> Forest PreOp
getDependenciesOnly = snd . runIdentity . evalDepForest1

opFromClosureB64 :: ByteString -> DevOp ()
opFromClosureB64 b64 = do
  let bstr = B64.decode b64
  let encodedClosure = either (error "invalid base64") id bstr
  unclosure $ Binary.decode encodedClosure

graphize :: Forest PreOp -> GraphData PreOp OpUniqueId
graphize forest = buildGraph preOpUniqueId forest

applyMethod :: [(Forest PreOp -> Forest PreOp)]
            -> Forest PreOp
            -> Method
            -> IO ()
applyMethod transformations originalForest method = do
  let forest = foldl (.) id transformations originalForest
  let graph = graphize forest

  case method of
    TurnUp   -> concurrentTurnup graph
    TurnDown -> concurrentTurndown graph
    Upkeep   -> concurrentUpkeep graph
    Print    -> display forest
    Dot      -> putStrLn . defaultDotify $ graph
    CheckDot -> putStrLn . dotifyWithStatuses graph =<< checkStatuses graph
    List     -> listUniqNodes forest
