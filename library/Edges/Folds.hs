module Edges.Folds
where

import Edges.Prelude
import Edges.Types
import Edges.Data
import qualified PrimitiveExtras.PrimArray as PrimArray
import qualified PrimitiveExtras.PrimMultiArray as PrimMultiArray


edgeCounts :: Amount a -> Fold (Node a, Node b) (EdgeCounts a b)
edgeCounts (Amount amountInt) =
  dimap edgeSourceIndex EdgeCounts (PrimArray.indexCountsFold amountInt)
  where
    edgeSourceIndex (Node sourceIndex, _) = sourceIndex

edges :: EdgeCounts a b -> Amount b -> Fold (Node a, Node b) (Edges a b)
edges (EdgeCounts edgeCountsPrimArray) (Amount bAmountInt) =
  dimap edgePair (Edges bAmountInt) (PrimMultiArray.fold edgeCountsPrimArray)
  where
    edgePair (Node sourceIndex, Node targetIndex) = (sourceIndex, fromIntegral targetIndex)
