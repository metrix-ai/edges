module Edges.Folds
where

import Edges.Prelude
import Edges.Types
import Control.Foldl
import qualified Data.IntMap.Strict as A
import qualified Data.HashMap.Strict as B


intCounts :: Fold Int (IntMap Int)
intCounts =
  Fold step init extract
  where
    step !state int = A.insertWith (+) int 1 state
    init = A.empty
    extract = id

indexHashMap :: (Eq key, Hashable key) => Fold key (HashMap key Int)
indexHashMap =
  Fold step init extract
  where
    init = (B.empty, 0)
    step (map, index) key =
      case B.lookup key map of
        Just _ -> (map, index)
        Nothing -> let
          newMap = B.insert key index map
          !newIndex = succ index
          in (newMap, newIndex)
    extract = fst

indexLookupTable :: (Eq node, Hashable node) => Fold node (IndexLookupTable node)
indexLookupTable =
  Fold step init extract
  where
    init = IndexLookupTable 0 B.empty
    step (IndexLookupTable index map) key =
      case B.lookup key map of
        Just _ -> IndexLookupTable index map
        Nothing -> let
          newMap = B.insert key index map
          newIndex = succ index
          in IndexLookupTable newIndex newMap
    extract = id

edgeCounts :: Int {-^ Amount of unique source nodes -} -> Fold (Edge from to) (EdgeCounts from to)
edgeCounts = error "TODO"

edges :: EdgeCounts from to -> Fold (Edge from to) (Edges from to)
edges = error "TODO"
