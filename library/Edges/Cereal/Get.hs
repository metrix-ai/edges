module Edges.Cereal.Get
where

import Edges.Prelude
import Edges.Types
import Data.Serialize.Get
import qualified Data.HashMap.Strict as A
import qualified Edges.HashMap as A
import qualified Data.Vector as B


getIndexHashMap :: (Eq k, Hashable k) => Get k -> Get (HashMap k Int)
getIndexHashMap getKey =
  getSize >>= getAssociations
  where
    getSize = getInt64le
    getAssociations size = foldM step A.empty (enumFromTo 0 (pred (fromIntegral size)))
      where
        step hashMap index = do
          key <- getKey
          return (A.insert key index hashMap)

getVector :: Get element -> Get (Vector element)
getVector getElement =
  getSize >>= getElements
  where
    getSize = getInt64le
    getElements size = B.replicateM (fromIntegral size) getElement

getNodeLookupTableFromIndexLookupTable :: Get node -> Get (NodeLookupTable node)
getNodeLookupTableFromIndexLookupTable getNode =
  NodeLookupTable <$> getVector getNode
