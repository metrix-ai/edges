{-# LANGUAGE PartialTypeSignatures #-}
module Edges.Folds
where

import Edges.Prelude
import Edges.Types
import Edges.UnliftedArray.FoldM
import qualified Control.Foldl as L
import qualified Data.IntMap.Strict as A
import qualified Data.HashMap.Strict as B
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV


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

type MutArr = UnliftedArray (MutableByteArray RealWorld)

mapUnlifted :: (a -> b) -> UnliftedArray a -> UnliftedArray b
mapUnlifted = undefined

edges :: forall from to . EdgeCounts from to
      -> Fold (Edge from to) (Edges from to)
edges (EdgeCounts vs) =
  let

    prealloc :: MutArr
    prealloc = unsafePerformIO $ L.foldM (sizedUnsafe (UV.length vs))
      (V.imap
        (\i len -> (i, unsafePerformIO . newByteArray $ fromIntegral $ 32 * len))
        (UV.convert vs))

    step :: MutArr -> Edge from to -> MutArr
    step = undefined

    final :: MutArr -> Edges from to
    final = Edges . MultiByteArray . mapUnlifted (unsafePerformIO . unsafeFreezeByteArray)

   in
    Fold step prealloc final
