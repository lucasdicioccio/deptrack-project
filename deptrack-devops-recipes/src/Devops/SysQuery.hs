
module Devops.SysQuery where

import           Data.Maybe     (catMaybes)
import           System.Process


type Prefix = String
type DeviceName = String
type SrcIp = String
data Route = Route !Prefix !DeviceName !SrcIp
  deriving Show

readRoutes :: IO [Route]
readRoutes = catMaybes . map parseRoute . lines <$> readProcess "ip" ["route"] ""
  where parseRoute :: String -> Maybe Route
        parseRoute = parseRouteTokens . words
        parseRouteTokens :: [String] -> Maybe Route
        parseRouteTokens ("default":_:ip:_:dev:_) = Just $ Route "default" dev ip
        parseRouteTokens (pfx:_:dev:_:_:_:_:_:ip:_) = Just $ Route pfx dev ip
        parseRouteTokens _ = Nothing

readDefaultRoute :: IO (Maybe Route)
readDefaultRoute = lookup "default" . formatPairs <$> readRoutes
  where formatPairs :: [Route] -> [(Prefix, Route)]
        formatPairs = map (\r@(Route pfx _ _) -> (pfx, r))
