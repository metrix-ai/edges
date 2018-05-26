module Edges.Types
where

import Edges.Prelude


data IndexLookupTable node = IndexLookupTable !Int !(HashMap node Int)

newtype NodeLookupTable node = NodeLookupTable (Vector node)


