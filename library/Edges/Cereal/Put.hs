module Edges.Cereal.Put
where

import Edges.Prelude
import Edges.Internal.Types
import Data.Serialize.Put
import PrimitiveExtras.Cereal.Put


nodeCounts :: Putter (NodeCounts entity)
nodeCounts (NodeCounts pa) =
  primArray putWord32le pa

edges :: Putter (Edges a b)
edges (Edges targetSpaceValue mpaValue) =
  targetSpace <> mpa
  where
    targetSpace = putInt64le (fromIntegral targetSpaceValue)
    mpa = primMultiArray putWord32le mpaValue
