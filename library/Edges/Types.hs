module Edges.Types
where

import Edges.Prelude hiding (fromList, toList)
import qualified Data.Vector.Unboxed as A
import qualified Data.Vector.Unboxed.Mutable as B
import qualified GHC.Exts as Exts ( IsList(..) )

data IndexLookupTable node = IndexLookupTable !Int !(HashMap node Int)

newtype NodeLookupTable node = NodeLookupTable (Vector node)

newtype MultiPrimArray a = MultiPrimArray (UnliftedArray (PrimArray a))
  deriving (Eq)

newtype Edges from to = Edges (MultiPrimArray Word32)
  deriving (Eq, Generic)

newtype Index node = Index Int

data Edge from to = Edge !Int !Int
  deriving (Generic)

newtype EdgeCounts from to = EdgeCounts (A.Vector Word32)

-- | Pair of mutable array and int vector. The latter is needed to track
-- next indices of mutable array during its sequential updates
data IdxVec =
  IdxVec
    !(B.MVector RealWorld Int)
    !(UnliftedArray (MutablePrimArray RealWorld Word32))

instance (Prim a, Serialize a) => Serialize (MultiPrimArray a) where
  put (MultiPrimArray bytearray) = putListOf (putListOf put . Exts.toList) (Exts.toList bytearray)
  get = (MultiPrimArray . Exts.fromList) <$> getListOf (Exts.fromList <$> getListOf get)

instance Serialize (Edge from to)
instance Serialize (Edges from to)
