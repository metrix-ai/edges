module Edges.Cereal.Instances
where

import Edges.Prelude
import Edges.Types
import Data.Serialize
import qualified Edges.Cereal.Get as A
import qualified Edges.Cereal.Put as B


instance Serialize (Node a) where
  get = A.node
  put = B.node

instance Serialize (NodeCounts a) where
  get = A.nodeCounts
  put = B.nodeCounts

instance Serialize (Edges a b) where
  get = A.edges
  put = B.edges
