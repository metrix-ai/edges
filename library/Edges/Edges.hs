module Edges.Edges
(
  Edges,
  list,
  listBipartite,
)
where

import Edges.Prelude
import Edges.Internal.Types
import qualified PrimitiveExtras.Monad as A
import qualified Control.Foldl as B
import qualified Control.Monad.Par as Par


list :: [(Node a, Node b)] -> Edges a b
list list =
  Par.runPar $ do
    aSizeFuture <- Par.spawnP $ succ $ maximum $ flip fmap list $ \ (Node x, _) -> x
    bSizeFuture <- Par.spawnP $ succ $ maximum $ flip fmap list $ \ (_, Node x) -> x
    aToBPrimFoldableFuture <- Par.spawnP $ flip fmap list $ \ (Node aInt, Node bInt) -> (aInt, fromIntegral bInt)
    aSize <- Par.get aSizeFuture
    bSize <- Par.get bSizeFuture
    aToBEdges <- primFoldableWithAmounts aSize bSize <$> Par.get aToBPrimFoldableFuture
    return aToBEdges

listBipartite :: [(Node a, Node b)] -> (Edges a b, Edges b a)
listBipartite list =
  Par.runPar $ do
    aSizeFuture <- Par.spawnP $ succ $ maximum $ flip fmap list $ \ (Node x, _) -> x
    bSizeFuture <- Par.spawnP $ succ $ maximum $ flip fmap list $ \ (_, Node x) -> x
    aToBPrimFoldableFuture <- Par.spawnP $ flip fmap list $ \ (Node aInt, Node bInt) -> (aInt, fromIntegral bInt)
    bToAPrimFoldableFuture <- Par.spawnP $ flip fmap list $ \ (Node aInt, Node bInt) -> (bInt, fromIntegral aInt)
    aSize <- Par.get aSizeFuture
    bSize <- Par.get bSizeFuture
    aToBEdgesFuture <- Par.spawn_ $ primFoldableWithAmounts aSize bSize <$> Par.get aToBPrimFoldableFuture
    bToAEdgesFuture <- Par.spawn_ $ primFoldableWithAmounts bSize aSize <$> Par.get bToAPrimFoldableFuture
    aToBEdges <- Par.get aToBEdgesFuture
    bToAEdges <- Par.get bToAEdgesFuture
    return (aToBEdges, bToAEdges)

primFoldableWithAmounts :: Foldable f => Int -> Int -> f (Int, Word32) -> Edges a b
primFoldableWithAmounts aAmount bAmount foldable =
  Edges bAmount $ runIdentity $ A.primMultiArray aAmount $ \ fold ->
  Identity $ B.fold fold foldable
