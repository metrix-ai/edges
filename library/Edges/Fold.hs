module Edges.Fold
where

import Edges.Prelude
import Edges.Types
import Edges.Node ()
import Edges.NodeCounts ()
import qualified PrimitiveExtras.PrimArray as PrimArray
import qualified PrimitiveExtras.PrimMultiArray as PrimMultiArray


edgeCounts :: Amount a -> Fold (Edge a b) (EdgeCounts a b)
edgeCounts (Amount amountInt) =
  dimap edgeSourceIndex EdgeCounts (PrimArray.indexCountsFold amountInt)
  where
    edgeSourceIndex (Edge sourceIndex _) = sourceIndex

edges :: EdgeCounts a b -> Amount b -> Fold (Edge a b) (Edges a b)
edges (EdgeCounts edgeCountsPrimArray) (Amount bAmountInt) =
  dimap edgePair (Edges bAmountInt) (PrimMultiArray.fold edgeCountsPrimArray)
  where
    edgePair (Edge sourceIndex targetIndex) = (sourceIndex, targetIndex)
