{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Edges.Types
where

import Edges.Prelude


data Edges source target = Edges Int {- Target array size -} (PrimMultiArray Word32)

newtype Node entity = Node Int

newtype NodeCounts entity = NodeCounts (PrimArray Word64)

newtype EdgeCounts source target = EdgeCounts (PrimArray Word64)

{-|
Total amount of unique entities of the type
-}
newtype Amount entity = Amount Int
