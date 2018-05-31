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
  Index . fromIntegral @Word32 <$> multiByteArrayAt mba indexPrim

countedTargets :: Edges source target -> Index source -> Unfold (Index target, Int)
countedTargets edges index =
  countingIndices (targets edges index)

countingInts :: Unfold Int -> Unfold (Int, Int)
countingInts =
  intMap . fold A.intMapCounts

countingIndices :: Unfold (Index a) -> Unfold (Index a, Int)
countingIndices =
  fmap (first Index) . countingInts . fmap (\ (Index x) -> x)

multiByteArrayIndices :: MultiByteArray -> Unfold Int
multiByteArrayIndices (MultiByteArray ua) =
  intsInRange 0 (pred (sizeofUnliftedArray ua))

multiByteArrayAt :: Prim prim => MultiByteArray -> Int -> Unfold prim
multiByteArrayAt (MultiByteArray ua) index =
  B.at ua index empty byteArrayPrims

byteArrayPrims :: Prim prim => ByteArray -> Unfold prim
byteArrayPrims ba =
  let
    !primSize =
      8 * sizeofByteArray ba
    in
      Unfold $ \ step init ->
      let
        loop index !state =
          if index < primSize
            then loop (succ index) (step state (indexByteArray ba index))
            else state
        in loop 0 init
