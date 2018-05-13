
-- | Set of constraints useful for declaring DevOp.
module Devops.Constraints where

import Control.Applicative (Alternative)
import Devops.Base

class HasOS a where
  os :: a -> String

type OSName = String

-- | Guards an action if it is not on a given OS.
onOS
  :: (Monad m, Applicative m, Alternative m, HasOS env)
  => OSName
  -> DevOpT env m a
  -> DevOpT env m a
onOS name act =
  guardEnv ((== name) . os) *> act
