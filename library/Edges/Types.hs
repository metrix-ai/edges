module Edges.Types
where

import Edges.Prelude


data IndexLookupTable node = IndexLookupTable !Int !(HashMap node Int)

newtype NodeLookupTable node = NodeLookupTable (Vector node)

newtype MultiByteArray = MultiByteArray (UnliftedArray ByteArray)

newtype Edges from to = Edges MultiByteArray

newtype Index node = Index Int

newtype ByIndex node value = ByIndex (IntMap value)
