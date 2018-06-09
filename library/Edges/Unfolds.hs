module Edges.Unfolds
where

import Edges.Prelude hiding (fold)
import Edges.Types
import DeferredFolds.Unfold
import qualified Edges.Folds as A
import qualified Edges.UnliftedArray as B
import qualified Data.IntMap.Strict as D


targets :: Edges source target -> Index source -> Unfold (Index target)
targets (Edges mba) (Index indexPrim) =
  Index . fromIntegral @Word32 <$> multiPrimArrayAt mba indexPrim

countedTargets :: Edges source target -> Index source -> Unfold (Index target, Int)
countedTargets edges index =
  countingIndices (targets edges index)

countingInts :: Unfold Int -> Unfold (Int, Int)
countingInts =
  intMap . fold A.intMapCounts

countingIndices :: Unfold (Index a) -> Unfold (Index a, Int)
countingIndices =
  fmap (first Index) . countingInts . fmap (\ (Index x) -> x)

multiPrimArrayIndices :: MultiPrimArray a -> Unfold Int
multiPrimArrayIndices (MultiPrimArray ua) =
  intsInRange 0 (pred (sizeofUnliftedArray ua))

multiPrimArrayAt :: Prim prim => MultiPrimArray prim -> Int -> Unfold prim
multiPrimArrayAt (MultiPrimArray ua) index =
  B.at ua index empty primArrayPrims

primArrayPrims :: Prim prim => PrimArray prim -> Unfold prim
primArrayPrims ba = Unfold $ \f z -> foldlPrimArray' f z ba
