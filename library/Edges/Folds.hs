module Edges.Folds
where

import Edges.Prelude
import Control.Foldl
import qualified Data.IntMap.Strict as A


intCounts :: Fold Int (IntMap Int)
intCounts =
  Fold step init extract
  where
    step !state int = A.insertWith (+) int 1 state
    init = A.empty
    extract = id
