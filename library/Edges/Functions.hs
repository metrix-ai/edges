module Edges.Functions
where

import Edges.Prelude
import Edges.Types
import qualified Data.Vector.Unboxed as UnboxedVector
import qualified DeferredFolds.UnfoldM as UnfoldM
import qualified Control.Foldl as Foldl
import qualified Control.Monad.Par as Par
import qualified PrimitiveExtras.PrimMultiArray as PrimMultiArray
import qualified PrimitiveExtras.PrimArray as PrimArray


edgesSourceAmount :: Edges source x -> Amount source
edgesSourceAmount (Edges _ pma) = Amount (PrimMultiArray.outerLength pma)

edgesTargetAmount :: Edges x target -> Amount target
edgesTargetAmount (Edges amount _) = Amount amount

edgesUnfoldM :: Monad m => Edges a b -> UnfoldM m (Node a, Node b)
edgesUnfoldM (Edges _ mpa) =
  fmap (\ (aInt, bWord32) -> (Node aInt, Node (fromIntegral bWord32))) $
  PrimMultiArray.toAssocsUnfoldM mpa

edgesList :: Edges a b -> [(Node a, Node b)]
edgesList edges =
  UnfoldM.fold Foldl.list (edgesUnfoldM edges)

listEdges :: [(Node a, Node b)] -> Edges a b
listEdges list =
  Par.runPar $ do
    aSizeFuture <- Par.spawnP $ succ $ fromMaybe 0 $ flip Foldl.fold list $ flip lmap Foldl.maximum $ \ (Node x, _) -> x
    bSizeFuture <- Par.spawnP $ succ $ fromMaybe 0 $ flip Foldl.fold list $ flip lmap Foldl.maximum $ \ (_, Node x) -> x
    aToBPrimFoldableFuture <- Par.spawnP $ flip fmap list $ \ (Node aInt, Node bInt) -> (aInt, fromIntegral bInt)
    aSize <- Par.get aSizeFuture
    bSize <- Par.get bSizeFuture
    aToBEdges <- primFoldableWithAmountsEdges aSize bSize <$> Par.get aToBPrimFoldableFuture
    return aToBEdges

listBipartiteEdges :: [(Node a, Node b)] -> (Edges a b, Edges b a)
listBipartiteEdges = coerce primListBipartiteEdges

primListBipartiteEdges :: [(Int, Int)] -> (Edges a b, Edges b a)
primListBipartiteEdges list =
  Par.runPar $ do
    aSizeFuture <- Par.spawnP $ succ $ fromMaybe 0 $ flip Foldl.fold list $ flip lmap Foldl.maximum fst
    bSizeFuture <- Par.spawnP $ succ $ fromMaybe 0 $ flip Foldl.fold list $ flip lmap Foldl.maximum snd
    aToBPrimFoldableFuture <- Par.spawnP $ flip fmap list $ \ (aInt, bInt) -> (aInt, fromIntegral bInt)
    bToAPrimFoldableFuture <- Par.spawnP $ flip fmap list $ \ (aInt, bInt) -> (bInt, fromIntegral aInt)
    aSize <- Par.get aSizeFuture
    bSize <- Par.get bSizeFuture
    aToBEdgesFuture <- Par.spawn_ $ primFoldableWithAmountsEdges aSize bSize <$> Par.get aToBPrimFoldableFuture
    bToAEdgesFuture <- Par.spawn_ $ primFoldableWithAmountsEdges bSize aSize <$> Par.get bToAPrimFoldableFuture
    aToBEdges <- Par.get aToBEdgesFuture
    bToAEdges <- Par.get bToAEdgesFuture
    return (aToBEdges, bToAEdges)

primFoldableWithAmountsEdges :: Foldable f => Int -> Int -> f (Int, Word32) -> Edges a b
primFoldableWithAmountsEdges aAmount bAmount foldable =
  Edges bAmount $ runIdentity $ PrimMultiArray.create aAmount $ \ fold ->
  Identity $ Foldl.fold fold foldable

nodeCountsList :: NodeCounts entity -> [Word32]
nodeCountsList (NodeCounts pa) = foldrPrimArray' (:) [] pa

nodeCountsUnboxedVector :: NodeCounts entity -> UnboxedVector.Vector Word32
nodeCountsUnboxedVector (NodeCounts pa) = PrimArray.toUnboxedVector pa
