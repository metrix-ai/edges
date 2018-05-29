module Edges.NodeLookupTable
(
  NodeLookupTable,
  indexLookupTable,
  lookup,
)
where

import Edges.Prelude hiding (lookup)
import Edges.Types
import qualified Edges.Vector as A
import qualified Data.Vector as B


indexLookupTable :: IndexLookupTable node -> NodeLookupTable node
indexLookupTable (IndexLookupTable size table) =
  NodeLookupTable vector
  where
    vector =
      A.indexHashMapWithSize size table

lookup :: Index node -> NodeLookupTable node -> Maybe node
lookup (Index indexPrim) (NodeLookupTable vector) =
  vector B.!? indexPrim
