module Edges.Potoki.Consume
where

import Edges.Prelude
import Edges.Types
import Potoki.Consume
import qualified Edges.Edges as A
import qualified Edges.MultiByteArray as B
import qualified Edges.UnliftedArray.Potoki.Consume as C


{-|
Construct index graph from arrays of indices, paired with their own indices.
-}
edges :: Int -> Consume (Int, ByteArray) Edges
edges size =
  Edges . MultiByteArray <$> C.sizedUnsafe size
