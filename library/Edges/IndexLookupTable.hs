module Edges.IndexLookupTable
(
  IndexLookupTable,
  lookup,
)
where

import Edges.Prelude hiding (lookup)
import Edges.Types
import qualified Data.HashMap.Strict as A


lookup :: (Eq node, Hashable node) => node -> IndexLookupTable node -> Maybe (Index node)
lookup node (IndexLookupTable size hashMap) =
  fmap Index (A.lookup node hashMap)
