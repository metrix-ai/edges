module Edges.NodeLookupTable
(
  NodeLookupTable,
  indexLookupTable,
)
where

import Edges.Prelude
import Edges.Types
import qualified Edges.Vector as A


indexLookupTable :: IndexLookupTable node -> NodeLookupTable node
indexLookupTable (IndexLookupTable size table) =
  NodeLookupTable vector
  where
    vector =
      A.indexHashMapWithSize size table
