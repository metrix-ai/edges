module Edges.Potoki.Produces
where

import Edges.Prelude
import Edges.Types
import Edges.Instances
import Potoki.Produce
import qualified Potoki.Cereal.Produce as Produce
import qualified Edges.Potoki.Transforms as Transforms


{-|
Enumerate nodes.
-}
sourceNodes :: Amount a -> Produce (Node a)
sourceNodes (Amount amountInt) = coerce (enumInRange 0 (pred amountInt))

{-|
Node counts paired with the source nodes.
-}
nodeCounts :: Amount a -> (Node a -> NodeCounts b) -> Produce (Node a, NodeCounts b)
nodeCounts amount nodeCounts =
  transform (Transforms.executeNodeCountQuery nodeCounts) (sourceNodes amount)

readNodeCountsFromFile :: FilePath -> Produce (Either IOException (Either Text (Node a, NodeCounts b)))
readNodeCountsFromFile file =
  Produce.fileDecoded file
