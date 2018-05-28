module Edges.MultiByteArray
where

import Edges.Prelude
import Edges.Types
import qualified Edges.ByteArray as C
import qualified Edges.UnliftedArray as B
import qualified DeferredFolds.Unfold as A


foldAt :: Prim prim => Int -> MultiByteArray -> Unfold prim
foldAt index (MultiByteArray ua) =
  B.at ua index empty C.fold

foldIndices :: MultiByteArray -> Unfold Int
foldIndices (MultiByteArray ua) =
  A.intsInRange 0 (pred (sizeofUnliftedArray ua))
