module Edges.Prelude
(
  module Exports,
  UnboxedVector,
  modifyTVar',
  forMToZero_,
  forMFromZero_,
  (.),
)
where


-- base
-------------------------
import Control.Applicative as Exports
import Control.Arrow as Exports
import Control.Category as Exports hiding ((.))
import Control.Concurrent as Exports
import Control.Exception as Exports
import Control.Monad as Exports hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Monad.IO.Class as Exports
import Control.Monad.Fix as Exports hiding (fix)
import Control.Monad.ST as Exports
import Data.Bits as Exports
import Data.Bool as Exports
import Data.Char as Exports
import Data.Coerce as Exports
import Data.Complex as Exports
import Data.Data as Exports
import Data.Dynamic as Exports
import Data.Either as Exports
import Data.Fixed as Exports
import Data.Foldable as Exports
import Data.Function as Exports hiding (id, (.))
import Data.Functor as Exports
import Data.Functor.Identity as Exports
import Data.Int as Exports
import Data.IORef as Exports
import Data.Ix as Exports
import Data.List as Exports hiding (sortOn, isSubsequenceOf, uncons, concat, foldr, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, find, maximumBy, minimumBy, mapAccumL, mapAccumR, foldl')
import Data.Maybe as Exports
import Data.Monoid as Exports hiding (Last(..), First(..))
import Data.Ord as Exports
import Data.Proxy as Exports
import Data.Ratio as Exports
import Data.STRef as Exports
import Data.String as Exports
import Data.Traversable as Exports
import Data.Tuple as Exports
import Data.Unique as Exports
import Data.Version as Exports
import Data.Word as Exports
import Debug.Trace as Exports
import Foreign.ForeignPtr as Exports
import Foreign.Ptr as Exports
import Foreign.StablePtr as Exports
import Foreign.Storable as Exports hiding (sizeOf, alignment)
import GHC.Conc as Exports hiding (withMVar, threadWaitWriteSTM, threadWaitWrite, threadWaitReadSTM, threadWaitRead)
import GHC.Exts as Exports (lazy, inline, sortWith, groupWith)
import GHC.Generics as Exports (Generic)
import GHC.IO.Exception as Exports
import Numeric as Exports
import Prelude as Exports hiding (concat, foldr, mapM_, sequence_, foldl1, maximum, minimum, product, sum, all, and, any, concatMap, elem, foldl, foldr1, notElem, or, mapM, sequence, id, (.))
import System.Environment as Exports
import System.Exit as Exports
import System.IO as Exports
import System.IO.Error as Exports
import System.IO.Unsafe as Exports
import System.Mem as Exports
import System.Mem.StableName as Exports
import System.Timeout as Exports
import Text.ParserCombinators.ReadP as Exports (ReadP, ReadS, readP_to_S, readS_to_P)
import Text.ParserCombinators.ReadPrec as Exports (ReadPrec, readPrec_to_P, readP_to_Prec, readPrec_to_S, readS_to_Prec)
import Text.Printf as Exports (printf, hPrintf)
import Text.Read as Exports (Read(..), readMaybe, readEither)
import Unsafe.Coerce as Exports

-- contravariant
-------------------------
import Data.Functor.Contravariant as Exports
import Data.Functor.Contravariant.Divisible as Exports

-- pointed
-------------------------
import Data.Pointed as Exports
import Data.Copointed as Exports

-- profunctors
-------------------------
import Data.Profunctor.Unsafe as Exports
import Data.Profunctor.Choice as Exports
import Data.Profunctor.Strong as Exports

-- semigroupoids
-------------------------
import Data.Semigroupoid as Exports

-- cereal
-------------------------
import Data.Serialize as Exports (Serialize)

-- deferred-folds
-------------------------
import DeferredFolds.Unfold as Exports (Unfold(..))
import DeferredFolds.UnfoldM as Exports (UnfoldM(..))

-- foldl
-------------------------
import Control.Foldl as Exports (Fold(..), FoldM(..))

-- primitive
-------------------------
import Data.Primitive as Exports
import Control.Monad.Primitive as Exports

-- vector
-------------------------
import Data.Vector as Exports (Vector)

-- primitive-extras
-------------------------
import PrimitiveExtras.Data as Exports

import qualified Data.Vector.Unboxed as UnboxedVector

type UnboxedVector = UnboxedVector.Vector

{-# INLINE modifyTVar' #-}
modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = do
    x <- readTVar var
    writeTVar var $! f x

{-# INLINE forMToZero_ #-}
forMToZero_ :: Applicative m => Int -> (Int -> m a) -> m ()
forMToZero_ !startN f =
  ($ pred startN) $ fix $ \loop !n -> if n >= 0 then f n *> loop (pred n) else pure ()

{-# INLINE forMFromZero_ #-}
forMFromZero_ :: Applicative m => Int -> (Int -> m a) -> m ()
forMFromZero_ !endN f =
  ($ 0) $ fix $ \loop !n -> if n < endN then f n *> loop (succ n) else pure ()

(.) :: Semigroupoid s => s b c -> s a b -> s a c
(.) = o
