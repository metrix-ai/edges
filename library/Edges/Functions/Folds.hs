module Edges.Functions.Folds
where

import Edges.Prelude
import Edges.Types
import Control.Foldl
import qualified Data.HashMap.Strict as HashMap


hashMapByKeyLookup :: (Eq b, Hashable b) => (a -> Maybe b) -> Fold a (HashMap b a)
hashMapByKeyLookup lookup = Fold (flip updateMap) init extract where
  updateMap a = case lookup a of
    Just b -> HashMap.insert b a
    Nothing -> id
  init = HashMap.empty
  extract = id

