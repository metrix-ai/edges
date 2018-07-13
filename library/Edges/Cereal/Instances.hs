module Edges.Cereal.Instances
where

import Edges.Prelude
import Edges.Internal.Types
import Data.Serialize
import qualified Edges.Cereal.Get as A
import qualified Edges.Cereal.Put as B


instance Serialize (NodeCounts a) where
  get = A.nodeCounts
  put = B.nodeCounts

instance Serialize (Edges a b) where
  get = A.edges
  put = B.edges
