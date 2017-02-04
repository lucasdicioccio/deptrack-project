{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Common types and utilities to describe OS and architectures
module Devops.OS where

import           Data.Proxy
import           Data.Typeable

-- | Known types of Operating Systems (more like Linux distros...)
data OS = Ubuntu14_04
        | Ubuntu16_04
        | CentOS7
        deriving (Eq,Show,Read,Typeable)

ubuntu14_04 :: Proxy 'Ubuntu14_04
ubuntu14_04 = Proxy

ubuntu16_04 :: Proxy 'Ubuntu16_04
ubuntu16_04 = Proxy

