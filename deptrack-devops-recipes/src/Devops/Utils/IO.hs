-- | Utilities related to IO subsystem
module Devops.Utils.IO (copy) where

import qualified Data.ByteString as BS
import           System.IO       (Handle)

-- | Copy data from one source handle to target handle
--
-- Probably better implemented using Pipes/Conduits/...
copy :: Handle -> Handle -> IO ()
copy hIn hOut = do
  bs <- BS.hGet hIn 4096
  if not (BS.null bs)
    then BS.hPut hOut bs >> copy hIn hOut
    else return ()
