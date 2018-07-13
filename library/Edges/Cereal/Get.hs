module Edges.Cereal.Get
where

import Edges.Prelude
import Edges.Internal.Types
import qualified PrimitiveExtras.Monad as A


nodeCounts :: Get (NodeCounts entity)
nodeCounts =
  do
    size <- getInt64le
    pa <- A.replicateMPrimArray (fromIntegral size) getWord32le
    return (NodeCounts pa)
