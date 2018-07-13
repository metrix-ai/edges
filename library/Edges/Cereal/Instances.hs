module Edges.Cereal.Instances
where

import Edges.Prelude
import Edges.Internal.Types
import qualified Edges.Cereal.Get as A
import qualified Edges.Cereal.Put as B


instance Serialize (NodeCounts a) where
  get = A.nodeCounts
  put = B.nodeCounts
