module Edges.Cereal.Put
where

import Edges.Prelude
import Edges.Types
import Data.Serialize.Put
import qualified PrimitiveExtras.PrimArray as PrimArray
import qualified PrimitiveExtras.PrimMultiArray as PrimMultiArray


int :: Putter Int
int = putInt64le . fromIntegral

nodeCounts :: Putter (NodeCounts entity)
nodeCounts (NodeCounts pa) =
  PrimArray.cerealPut int putWord32le pa

edges :: Putter (Edges a b)
edges (Edges targetSpaceValue mpaValue) = targetSpace <> mpa where
  targetSpace = int targetSpaceValue
  mpa = PrimMultiArray.cerealPut int putWord32le mpaValue

node :: Putter (Node a)
node (Node x) = int x
