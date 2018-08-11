module Edges.Cereal.Get
where

import Edges.Prelude
import Edges.Types
import Data.Serialize
import qualified PrimitiveExtras.PrimArray as PrimArray
import qualified PrimitiveExtras.PrimMultiArray as PrimMultiArray


nodeCounts :: Get (NodeCounts entity)
nodeCounts = NodeCounts <$> get

edges :: Get (Edges a b)
edges = do
  targetSpace <- get
  pma <- PrimMultiArray.cerealGetAsInMemory get
  return (Edges targetSpace pma)

node :: Get (Node a)
node = Node <$> get
