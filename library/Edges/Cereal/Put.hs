module Edges.Cereal.Put
where

import Edges.Prelude
import Edges.Types
import Data.Serialize
import qualified PrimitiveExtras.PrimArray as PrimArray
import qualified PrimitiveExtras.PrimMultiArray as PrimMultiArray


nodeCounts :: Putter (NodeCounts entity)
nodeCounts (NodeCounts pa) =
  PrimArray.cerealPutAsInMemory put pa

edges :: Putter (Edges a b)
edges (Edges targetSpaceValue mpaValue) = targetSpace <> mpa where
  targetSpace = put targetSpaceValue
  mpa = PrimMultiArray.cerealPutAsInMemory put mpaValue

node :: Putter (Node a)
node (Node x) = put x
