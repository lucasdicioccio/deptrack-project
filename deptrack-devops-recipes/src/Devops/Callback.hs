
module Devops.Callback where

type SelfPath = FilePath

-- | Method to callback a non-local node.
-- TODO: develop on this to be able to represent parasited/chrooted calls
--       ideally we also need a way to "link" long-lived processes such as Backends together
data CallBackMethod =
    NoCallBack
  | BinaryCall !FilePath ![String]
