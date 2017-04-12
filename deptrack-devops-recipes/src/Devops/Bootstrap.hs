{-# LANGUAGE TypeFamilies #-}
-- | Defines `DevOp` dependencies for managing *nodes*
--
-- Nodes are systems which can be containers, Qemu images, remote hosts, cloud VMs and to which
-- a `Parasite` can be deployed for inclusion in a global DepTrack graph.
-- This module exposes basic types for defining such nodes, import specific sub-modules for defining
-- provider-dependent parameters.
module Devops.Bootstrap where

-- | An instance of node, parameterized by the type of underlying provider.
-- A node is defined by its configuration which depends on its type
data Node a = Node { nodeConfig :: !(NodeConfig a) }

-- | Defines configuration for some type of `Node`
type family NodeConfig a :: *
