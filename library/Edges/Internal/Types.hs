module Edges.Internal.Types
where

import Edges.Prelude


data Edges source target =
  Edges
    {-# UNPACK #-} !Int {-^ Target array size -}
    {-# UNPACK #-} !(PrimMultiArray Word32)

newtype Index entity = Index Int

newtype IndexCounts entity = IndexCounts (PrimArray Word32)

data Count source target =
  Count
    {-# UNPACK #-} !Int {-^ Target count array size -}
    (Int {-^ Index -} -> Word32 {-^ Count -} -> TVarArray Word32 {-^ Target count array -} -> IO ())
