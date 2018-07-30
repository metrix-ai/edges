module Edges.Cereal.Get
where

import Edges.Prelude
import Edges.Types
import Data.Serialize.Get
import qualified PrimitiveExtras.PrimArray as PrimArray
import qualified PrimitiveExtras.PrimMultiArray as PrimMultiArray


int :: Get Int
int = fromIntegral <$> getInt64le

nodeCounts :: Get (NodeCounts entity)
nodeCounts =
  NodeCounts <$> PrimArray.cerealGet int getWord32le

edges :: Get (Edges a b)
edges = do
  targetSpace <- int
  pma <- PrimMultiArray.cerealGet int getWord32le
  return (Edges targetSpace pma)

node :: Get (Node a)
node = Node <$> int
