module Edges.Potoki.Produce
where

import Edges.Prelude
import Edges.Types
import Edges.NodeCounts ()
import Potoki.Produce
import qualified PrimitiveExtras.PrimMultiArray as PrimMultiArray
import qualified Potoki.Transform as B
import qualified Potoki.Cereal.Produce as C


nodes :: Edges a x -> Produce (Node a)
nodes (Edges _ pma) =
  coerce $
  enumInRange 0 (pred (PrimMultiArray.outerLength pma))

nodeCounts :: Edges a x -> (Node a -> NodeCounts b) -> Produce (Node a, NodeCounts b)
nodeCounts edges nodeCounts =
  transform (B.concurrently numCapabilities (arr (\ node -> case nodeCounts node of x -> (node, x)))) $
  nodes edges

nodeCountsFromFile :: FilePath -> Produce (Either IOException (Either Text (Node a, NodeCounts b)))
nodeCountsFromFile file =
  C.fileDecoded file
