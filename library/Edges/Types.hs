module Edges.Types
where

import Edges.Prelude


newtype IndexLookupTable node = IndexLookupTable (HashMap node Int)

newtype NodeLookupTable node = NodeLookupTable (Vector node)


