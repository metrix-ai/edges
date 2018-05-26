module Edges.Potoki.Consume
where

import Edges.Prelude
import Potoki.Consume
import qualified Edges.Edges as A
import qualified Edges.MultiByteArray as B
import qualified Edges.UnliftedArray.Potoki.Consume as C


{-|
Construct index graph from arrays of indices, paired with their own indices.
-}
indexGraph :: Int -> Consume (Int, ByteArray) A.Edges
indexGraph size =
  A.Edges . B.MultiByteArray <$> C.sizedUnsafe size
