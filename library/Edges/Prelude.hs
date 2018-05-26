module Edges.Prelude
(
  module Exports,
)
where


-- base
-------------------------
import Prelude as Exports hiding ((<>), sizeOf)
import Data.Functor as Exports
import Data.Monoid as Exports hiding ((<>), First(..), Last(..))
import Data.Semigroup as Exports
import Data.Foldable as Exports
import Data.Traversable as Exports
import Control.Applicative as Exports
import Control.Monad as Exports

-- containers
-------------------------
import Data.IntMap.Strict as Exports (IntMap)

-- foldl
-------------------------
import Control.Foldl as Exports (Fold(..), FoldM(..))

-- deferred-folds
-------------------------
import DeferredFolds.FoldlView as Exports (FoldlView(..))

-- primitive
-------------------------
import Data.Primitive as Exports
import Data.Primitive.UnliftedArray as Exports
import Control.Monad.Primitive as Exports
