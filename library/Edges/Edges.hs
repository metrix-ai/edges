module Edges.Edges
where

import Edges.Prelude
import qualified Edges.Folds as A
import qualified Edges.MultiByteArray as B
import qualified DeferredFolds.Unfold as C


newtype Edges = Edges B.MultiByteArray

foldAt :: Int -> Edges -> Unfold Int
foldAt index (Edges mba) =
  B.foldAt index mba

foldDeepAt :: Int -> Int -> Edges -> Unfold Int
foldDeepAt depth index graph =
  if depth < 1
    then mzero
    else do
      deeperIndex <- foldAt index graph
      foldDeepAt (pred depth) deeperIndex graph

foldDeep :: Int -> Edges -> Unfold Int
foldDeep depth graph =
  do
    index <- foldAll graph
    foldDeepAt depth index graph

foldAll :: Edges -> Unfold Int
foldAll (Edges mba) =
  B.foldIndices mba

foldDeepCounts :: Int -> Edges -> Unfold (Int, IntMap Int)
foldDeepCounts depth graph =
  do
    index <- foldAll graph
    return (index, countDeepAt depth index graph)

{-|
Index-to-frequency map of nodes positioned at the specified search depth by the specified node index.
-}
countDeepAt :: Int -> Int -> Edges -> IntMap Int
countDeepAt depth index =
  C.fold A.intCounts .
  foldDeepAt depth index
