module Edges.Potoki.Produce
where

import Edges.Prelude
import Edges.Types
import Potoki.Produce
import qualified PrimitiveExtras.Pure as A


nodes :: Edges a x -> Produce (Node a)
nodes (Edges _ pma) =
  fmap Node $
  enumInRange 0 (pred (A.primMultiArrayOuterLength pma))

nodeCounts :: Edges a x -> (Node a -> NodeCounts b) -> Produce (Node a, NodeCounts b)
nodeCounts edges nodeCounts =
  fmap (\ node -> case nodeCounts node of x -> (node, x)) $
  nodes edges
