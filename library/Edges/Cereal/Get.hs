module Edges.Cereal.Get
where

import Edges.Prelude
import Edges.Types
import Data.Serialize.Get
import PrimitiveExtras.Cereal.Get


nodeCounts :: Get (NodeCounts entity)
nodeCounts =
  NodeCounts <$> primArray getWord32le

edges :: Get (Edges a b)
edges =
  do
    targetSpace <- fromIntegral <$> getInt64le
    pma <- primMultiArray getWord32le
    return (Edges targetSpace pma)

node :: Get (Node a)
node = Node . fromIntegral <$> getInt64le
