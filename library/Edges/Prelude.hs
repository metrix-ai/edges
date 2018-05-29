module Edges.Prelude
(
  module Exports,
)
where


-- base
-------------------------
import Prelude as Exports hiding ((<>), sizeOf)
import Data.Word as Exports
import Data.Bifunctor as Exports
import Data.Functor as Exports
import Data.Monoid as Exports hiding ((<>), First(..), Last(..))
import Data.Semigroup as Exports
import Data.Foldable as Exports
import Data.Traversable as Exports
import Control.Applicative as Exports
import Control.Monad as Exports
import System.IO.Unsafe as Exports

-- containers
-------------------------
import Data.IntMap.Strict as Exports (IntMap)

-- deferred-folds
-------------------------
import DeferredFolds.Unfold as Exports (Unfold(..))

-- foldl
-------------------------
import Control.Foldl as Exports (Fold(..), FoldM(..))

-- hashable
-------------------------
import Data.Hashable as Exports (Hashable(..))

-- primitive
-------------------------
import Data.Primitive as Exports
import Data.Primitive.UnliftedArray as Exports
import Control.Monad.Primitive as Exports

-- unordered-containers
-------------------------
import Data.HashMap.Strict as Exports (HashMap)

-- vector
-------------------------
import Data.Vector as Exports (Vector)
