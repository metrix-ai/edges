module Edges.Cereal.Put
where

import Edges.Prelude
import Edges.Types
import Data.Serialize.Put
import qualified Data.HashMap.Strict as A
import qualified Edges.HashMap as A
import qualified Data.Vector as B


putHashMapWithSize :: Putter k -> Putter v -> Putter (HashMap k v)
putHashMapWithSize keyPutter valuePutter hashMap =
  size *> associations
  where
    size = putInt64le (fromIntegral (A.size hashMap))
    associations = A.traverse_ association hashMap
    association key value = keyPutter key *> valuePutter value

putVector :: Putter element -> Putter (Vector element)
putVector putElement vector =
  putSize *> putElements
  where
    putSize = putInt64le (fromIntegral (B.length vector))
    putElements = traverse_ putElement vector

{-|
It's suggested to use 'putNodeLookupTable' instead.
The hashmap traversal implementation is inefficient and
the representation of 'NodeLookupTable' is more compact.
-}
putIndexLookupTable :: Putter node -> Putter (IndexLookupTable node)
putIndexLookupTable putNode (IndexLookupTable size hashMap) =
  putSize *> putAssociations
  where
    putSize = putInt64le (fromIntegral size)
    putAssociations = A.traverse_ putAssociation hashMap
    putAssociation key value = putNode key *> putInt64le (fromIntegral value)

putNodeLookupTable :: Putter node -> Putter (NodeLookupTable node)
putNodeLookupTable putNode (NodeLookupTable vector) =
  putVector putNode vector
