module Edges.Potoki.Transforms
where

import Edges.Prelude
import Edges.Types
import Edges.Instances
import Potoki.Transform


{-|
Node counts paired with the source nodes.
-}
executeNodeCountQuery :: (Node a -> NodeCounts b) -> Transform (Node a) (Node a, NodeCounts b)
executeNodeCountQuery nodeCounts =
  concurrently numCapabilities (arr (\ node -> case nodeCounts node of x -> (node, x)))
