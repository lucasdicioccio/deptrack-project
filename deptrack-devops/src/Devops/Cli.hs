{-# LANGUAGE RecordWildCards #-}
-- | Building-block methods to build a command-line tool able to inspect and
-- turnup/turndown DevOps.
module Devops.Cli (
    Method (..)
  , applyMethod
  , getDependenciesOnly
  , opFromClosureB64
  , graphize
  , defaultMain
  , App (..)
  , appMain
  , appMethod
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
import           System.Environment          (getArgs, getExecutablePath)

import           Devops.Actions              (concurrentTurndown,
                                              concurrentTurnup, concurrentUpkeep, display, defaultDotify,
                                              dotifyWithStatuses,
                                              listUniqNodes, checkStatuses)
import           Devops.Base                 (DevOp, OpUniqueId, PreOp, preOpUniqueId)

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
applyMethod transformations originalForest meth = do
  let forest = foldl (.) id transformations originalForest
  let graph = graphize forest

  case meth of
    TurnUp   -> concurrentTurnup graph
    TurnDown -> concurrentTurndown graph
    Upkeep   -> concurrentUpkeep graph
    Print    -> display forest
    Dot      -> putStrLn . defaultDotify $ graph
    CheckDot -> putStrLn . dotifyWithStatuses graph =<< checkStatuses graph
    List     -> listUniqNodes forest

defaultMain :: DevOp a
            -- ^ an operation
            -> [(Forest PreOp -> Forest PreOp)]
            -- ^ forest transformations to optimize the resulting graph
            -> [String]
            -- ^ args
            -> IO ()
defaultMain devop optimizations = go
  where
    forest = getDependenciesOnly devop
    call m = applyMethod optimizations forest m
    go ("up":_)        = call TurnUp
    go ("down":_)      = call TurnDown
    go ("upkeep":_)    = call Upkeep
    go ("print":_)     = call Print
    go ("dot":_)       = call Dot
    go ("check-dot":_) = call CheckDot
    go ("list":_)      = call List
    go _ = putStrLn usage
    usage = unlines [ "deptrack-devops default main:"
                    , "  Available arguments:"
                    , "    up, down, upkeep, print, dot, check-dot, list"
                    ]

-- | An optimization on PreOp forests.
type ForestOptimization = Forest PreOp -> Forest PreOp

-- | A FilePath corresponding to the file with the currently-executing binary.
type SelfPath = FilePath

-- | A builder for app that can be useful for defining an infrastructure as a
-- recursive structure where the "main entry point" of the recursion is the
-- binary itself.
data App arch = App {
    _args   :: [String] -> (arch, Method)
  -- ^ Parses arguments, returns a parsed architecture and a set of args for
  -- the real defaulMain.
  , _target :: arch -> SelfPath -> DevOp ()
  -- ^ Generates a target from the argument and the selfPath
  , _opts   :: [ForestOptimization]
  }

appMain :: App a -> IO ()
appMain App{..} = do
  self <- getExecutablePath
  args <- getArgs
  let (arch, meth) = _args args
  let forest = getDependenciesOnly $ _target arch self
  applyMethod _opts forest meth

appMethod :: String -> Method
appMethod "up"        = TurnUp
appMethod "down"      = TurnDown
appMethod "upkeep"    = Upkeep
appMethod "print"     = Print
appMethod "dot"       = Dot
appMethod "check-dot" = CheckDot
appMethod "list"      = List
appMethod str         = error $ "unparsed appMethod: " ++ str
