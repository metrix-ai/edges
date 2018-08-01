module Edges.Gens
where

import Edges.Prelude hiding (choose)
import Edges.Data
import Test.QuickCheck.Gen


nodeWithLimit :: Int -> Gen (Node a)
nodeWithLimit max = Node <$> choose (0, max)

bipartiteEdgesWithLimits :: Int -> Int -> Gen (Edges a b, Edges b a)
bipartiteEdgesWithLimits nodeLimit edgeLimit =
  do
    aMaxIndex <- choose (0, pred nodeLimit)
    bMaxIndex <- choose (0, pred nodeLimit)
    edgesAmount <- choose (0, edgeLimit)
    if aMaxIndex == 0 || bMaxIndex == 0
      then return (primListBipartiteEdges [])
      else do
        edges <- replicateM edgesAmount $ (,) <$> choose (0, aMaxIndex) <*> choose (0, bMaxIndex)
        return (primListBipartiteEdges edges)
