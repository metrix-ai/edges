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

getIndexLookupTable :: (Eq node, Hashable node) => Get node -> Get (IndexLookupTable node)
getIndexLookupTable getNode =
  do
    size <- getSize
    associations <- getAssociations size
    return (IndexLookupTable size associations)
  where
    getSize = fromIntegral <$> getInt64le
    getAssociations size = foldM step A.empty (enumFromTo 0 (pred size))
      where
        step hashMap index = do
          node <- getNode
          return (A.insert node index hashMap)

getVector :: Get element -> Get (Vector element)
getVector getElement =
  getSize >>= getElements
  where
    getSize = getInt64le
    getElements size = B.replicateM (fromIntegral size) getElement

getNodeLookupTable :: Get node -> Get (NodeLookupTable node)
getNodeLookupTable getNode =
  NodeLookupTable <$> getVector getNode
