module Edges.Unfolds
where

import Edges.Prelude hiding (fold)
import Edges.Types
import DeferredFolds.Unfold
import qualified Edges.Folds as A
import qualified Edges.MultiByteArray as B
import qualified Data.IntMap.Strict as D


targets :: Edges source target -> Index source -> Unfold (Index target)
targets (Edges mba) (Index indexPrim) =
  Index . fromIntegral @Word32 <$> B.foldAt indexPrim mba

countedTargets :: Edges source target -> Index source -> Unfold (Index target, Int)
countedTargets edges index =
  countingIndices (targets edges index)

countingInts :: Unfold Int -> Unfold (Int, Int)
countingInts =
  intMap . fold A.intCounts

countingIndices :: Unfold (Index a) -> Unfold (Index a, Int)
countingIndices =
  fmap (first Index) . countingInts . fmap (\ (Index x) -> x)
