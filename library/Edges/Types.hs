module Edges.Types
where

import Edges.Prelude


data Edges source target =
  Edges
    {-# UNPACK #-} !Int {-^ Target array size -}
    {-# UNPACK #-} !(PrimMultiArray Word32)

newtype Node entity = Node Int

newtype NodeCounts entity = NodeCounts (PrimArray Word32)
