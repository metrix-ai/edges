module Edges.Types
where

import Edges.Prelude
import qualified Data.Vector.Unboxed as A
import qualified Data.Vector.Unboxed.Mutable as B


data IndexLookupTable node = IndexLookupTable !Int !(HashMap node Int)

newtype NodeLookupTable node = NodeLookupTable (Vector node)

newtype MultiByteArray = MultiByteArray (UnliftedArray ByteArray)

newtype Edges from to = Edges MultiByteArray

newtype Index node = Index Int

data Edge from to = Edge !Int !Int
  deriving (Generic)

newtype EdgeCounts from to = EdgeCounts (A.Vector Word32)

-- | Pair of mutable array and int vector. The latter is needed to track
-- next indices of mutable array during its sequential updates
data IdxVec =
  IdxVec
    !(B.MVector RealWorld Int)
    !(UnliftedArray (MutableByteArray RealWorld))

instance Serialize (Edge from to)
