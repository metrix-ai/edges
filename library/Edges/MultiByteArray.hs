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

foldAtWord32 :: Word32 -> MultiByteArray -> Unfold Word32
foldAtWord32 index (MultiByteArray ua) =
  B.at ua (fromIntegral index) empty C.fold

foldIndices :: MultiByteArray -> Unfold Int
foldIndices (MultiByteArray ua) =
  A.intsInRange 0 (pred (sizeofUnliftedArray ua))
