module Edges.Edges
(
  Edges,
  list,
  listBipartite,
  primListBipartite,
  toAssocUnfoldM,
  toAssocList,
)
where

import Edges.Prelude
import Edges.Types
import Edges.Cereal.Instances ()
import qualified PrimitiveExtras.Monad as Monad
import qualified Control.Foldl as Foldl
import qualified Control.Monad.Par as Par
import qualified PrimitiveExtras.UnfoldM as UnfoldM
import qualified DeferredFolds.UnfoldM as UnfoldM


list :: [(Node a, Node b)] -> Edges a b
list list =
  Par.runPar $ do
    aSizeFuture <- Par.spawnP $ succ $ fromMaybe 0 $ flip Foldl.fold list $ flip lmap Foldl.maximum $ \ (Node x, _) -> x
    bSizeFuture <- Par.spawnP $ succ $ fromMaybe 0 $ flip Foldl.fold list $ flip lmap Foldl.maximum $ \ (_, Node x) -> x
    aToBPrimFoldableFuture <- Par.spawnP $ flip fmap list $ \ (Node aInt, Node bInt) -> (aInt, fromIntegral bInt)
    aSize <- Par.get aSizeFuture
    bSize <- Par.get bSizeFuture
    aToBEdges <- primFoldableWithAmounts aSize bSize <$> Par.get aToBPrimFoldableFuture
    return aToBEdges

listBipartite :: [(Node a, Node b)] -> (Edges a b, Edges b a)
listBipartite = coerce primListBipartite

primListBipartite :: [(Int, Int)] -> (Edges a b, Edges b a)
primListBipartite list =
  Par.runPar $ do
    aSizeFuture <- Par.spawnP $ succ $ fromMaybe 0 $ flip Foldl.fold list $ flip lmap Foldl.maximum fst
    bSizeFuture <- Par.spawnP $ succ $ fromMaybe 0 $ flip Foldl.fold list $ flip lmap Foldl.maximum snd
    aToBPrimFoldableFuture <- Par.spawnP $ flip fmap list $ \ (aInt, bInt) -> (aInt, fromIntegral bInt)
    bToAPrimFoldableFuture <- Par.spawnP $ flip fmap list $ \ (aInt, bInt) -> (bInt, fromIntegral aInt)
    aSize <- Par.get aSizeFuture
    bSize <- Par.get bSizeFuture
    aToBEdgesFuture <- Par.spawn_ $ primFoldableWithAmounts aSize bSize <$> Par.get aToBPrimFoldableFuture
    bToAEdgesFuture <- Par.spawn_ $ primFoldableWithAmounts bSize aSize <$> Par.get bToAPrimFoldableFuture
    aToBEdges <- Par.get aToBEdgesFuture
    bToAEdges <- Par.get bToAEdgesFuture
    return (aToBEdges, bToAEdges)

primFoldableWithAmounts :: Foldable f => Int -> Int -> f (Int, Word32) -> Edges a b
primFoldableWithAmounts aAmount bAmount foldable =
  Edges bAmount $ runIdentity $ Monad.primMultiArray aAmount $ \ fold ->
  Identity $ Foldl.fold fold foldable

toAssocUnfoldM :: Monad m => Edges a b -> UnfoldM m (Node a, Node b)
toAssocUnfoldM (Edges _ mpa) =
  fmap (\ (aInt, bWord32) -> (Node aInt, Node (fromIntegral bWord32))) $
  UnfoldM.primMultiArrayAssocs mpa

toAssocList :: Edges a b -> [(Node a, Node b)]
toAssocList edges =
  UnfoldM.fold Foldl.list (toAssocUnfoldM edges)
