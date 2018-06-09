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
import qualified Data.Vector.Unboxed.Mutable as UVM


intMapCounts :: Fold Int (IntMap Int)
intMapCounts =
  Fold step init extract
  where
    step !state int = A.insertWith (+) int 1 state
    init = A.empty
    extract = id

intCounts :: Int {-^ Vector size -} -> Fold Int {-^ Index -} (UV.Vector Word32)
intCounts size = Fold step begin final where
  step ics i = unsafePerformIO $ ics <$ UVM.modify ics succ i
  begin = unsafePerformIO $ UVM.replicate size 0
  final = unsafePerformIO . UV.unsafeFreeze

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
edgeCounts = L.premap (\ (Edge from _) -> from) . fmap EdgeCounts . intCounts

edges :: forall from to . EdgeCounts from to
      -> Fold (Edge from to) (Edges from to)
edges (EdgeCounts vs) =
  let

    prealloc :: IdxVec
    prealloc = unsafePerformIO $ IdxVec
      <$> UVM.replicate (UV.length vs) 0
      <*> (L.foldM
        (sizedUnsafe $ UV.length vs)
        (V.imap
          (\i len -> (i, unsafePerformIO . newPrimArray $ fromIntegral len))
          (UV.convert vs)))

    step :: IdxVec -> Edge from to -> IdxVec
    step acc@(IdxVec indices graph) (Edge a b) = unsafePerformIO $ do
      let neighbs = indexUnliftedArray graph a
      i <- UVM.read indices a
      writePrimArray neighbs i $ fromIntegral b  -- write at next index
      UVM.write indices a $ succ i               -- update next index
      return acc

    final :: IdxVec -> Edges from to
    final (IdxVec _ graph) = unsafePerformIO $ do
      -- copy to mutable, freeze rows then freeze outer
      let size = sizeofUnliftedArray graph
      copied <- unsafeNewUnliftedArray size
      for_ ([0..size - 1] :: [Int]) $ \i -> do
        let mutable = indexUnliftedArray graph i
        writeUnliftedArray copied i =<< unsafeFreezePrimArray mutable
      result <- unsafeFreezeUnliftedArray copied
      return $ Edges . MultiPrimArray $ result
   in
    Fold step prealloc final
