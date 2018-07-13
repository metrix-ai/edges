module Edges.Cereal.Put
where

import Edges.Prelude
import Edges.Internal.Types
import qualified PrimitiveExtras.Monad as A


nodeCounts :: Putter (NodeCounts entity)
nodeCounts (NodeCounts pa) =
  size <> elements
  where
    size = putInt64le (fromIntegral (sizeofPrimArray pa))
    elements = flip traversePrimArray_ pa $ \ element -> putWord32le element
