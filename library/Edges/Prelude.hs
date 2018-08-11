module Edges.Prelude
(
  module Exports,
  UnboxedVector,
  modifyTVar',
  forMInAscendingRange_,
  forMInDescendingRange_,
  showText,
  -- * Optics
  Lens,
  Lens',
  Prism,
  Prism',
  lens,
  prism,
  _Left,
  _Just,
)
where


-- base
-------------------------
import Control.Applicative as Exports
import Control.Arrow as Exports
import Control.Category as Exports
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

-- hashable
-------------------------
import Data.Hashable as Exports (Hashable)

-- primitive
-------------------------
import Data.Primitive as Exports
import Control.Monad.Primitive as Exports

-- primitive-extras
-------------------------
import PrimitiveExtras.PrimMultiArray as Exports (PrimMultiArray)
import PrimitiveExtras.TVarArray as Exports (TVarArray)

-- unordered-containers
-------------------------
import Data.HashMap.Strict as Exports (HashMap)

-- vector
-------------------------
import Data.Vector as Exports (Vector)

-- text
-------------------------
import Data.Text as Exports (Text)

-- deepseq
-------------------------
import Control.DeepSeq as Exports

import qualified Data.Vector.Unboxed as UnboxedVector

type UnboxedVector = UnboxedVector.Vector

{-# INLINE modifyTVar' #-}
modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' var f = do
    x <- readTVar var
    writeTVar var $! f x

{-# INLINE forMInAscendingRange_ #-}
forMInAscendingRange_ :: Applicative m => Int -> Int -> (Int -> m a) -> m ()
forMInAscendingRange_ !startN !endN f =
  ($ startN) $ fix $ \loop !n -> if n < endN then f n *> loop (succ n) else pure ()

{-# INLINE forMInDescendingRange_ #-}
forMInDescendingRange_ :: Applicative m => Int -> Int -> (Int -> m a) -> m ()
forMInDescendingRange_ !startN !endN f =
  ($ pred startN) $ fix $ \loop !n -> if n >= endN then f n *> loop (pred n) else pure ()

showText = fromString . show


-- * Optics
-------------------------

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

{-# INLINE lens #-}
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)

{-# INLINE prism #-}
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta = dimap seta (either pure (fmap bt)) . right'

{-# INLINE _Left #-}
_Left :: Prism' (Either a b) a
_Left = prism Left (either Right (Left . Right))

{-# INLINE _Just #-}
_Just :: Prism' (Maybe a) a
_Just = prism Just (maybe (Left Nothing) Right)
