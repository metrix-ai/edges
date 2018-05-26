module Edges.Cereal.Put
where

import Edges.Prelude
import Edges.Types
import Data.Serialize.Put
import qualified Data.HashMap.Strict as A
import qualified Edges.HashMap as A


hashMapWithSize :: Putter k -> Putter v -> Putter (HashMap k v)
hashMapWithSize keyPutter valuePutter hashMap =
  size *> associations
  where
    size = putInthost (A.size hashMap)
    associations = A.traverse_ association hashMap
    association key value = keyPutter key *> valuePutter value

indexLookupTable :: Putter node -> Putter (IndexLookupTable node)
indexLookupTable putNode (IndexLookupTable size hashMap) =
  putSize *> putAssociations
  where
    putSize = putInthost size
    putAssociations = A.traverse_ putAssociation hashMap
    putAssociation key value = putNode key *> putInthost value
