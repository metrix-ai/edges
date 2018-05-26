module Edges.MultiByteArray
where

import Edges.Prelude
import qualified Edges.ByteArray as C
import qualified Edges.UnliftedArray as B
import qualified DeferredFolds.FoldlView as A


newtype MultiByteArray = MultiByteArray (UnliftedArray ByteArray)

foldAt :: Prim prim => Int -> MultiByteArray -> FoldlView prim
foldAt index (MultiByteArray ua) =
  B.at ua index empty C.fold

foldIndices :: MultiByteArray -> FoldlView Int
foldIndices (MultiByteArray ua) =
  A.intsInRange 0 (pred (sizeofUnliftedArray ua))
