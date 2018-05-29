module Edges.Types
where

import Edges.Prelude
import qualified Data.Vector.Unboxed as A


data IndexLookupTable node = IndexLookupTable !Int !(HashMap node Int)

newtype NodeLookupTable node = NodeLookupTable (Vector node)

newtype MultiByteArray = MultiByteArray (UnliftedArray ByteArray)

newtype Edges from to = Edges MultiByteArray

newtype Index node = Index Int

data Edge from to = Edge !Int !Int

newtype EdgeCounts from to = EdgeCounts (A.Vector Word32)
