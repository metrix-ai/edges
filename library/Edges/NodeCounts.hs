module Edges.NodeCounts
(
  NodeCounts,
  node,
  nodeTargets,
  targets,
  toList,
  toUnboxedVector,
)
where

import Edges.Prelude hiding (index, toList)
import Edges.Types
import Edges.Cereal.Instances ()
import qualified PrimitiveExtras.UnfoldM as A
import qualified PrimitiveExtras.Pure as C
import qualified PrimitiveExtras.IO as D
import qualified PrimitiveExtras.Fold as E
import qualified DeferredFolds.UnfoldM as B
import qualified Data.Vector.Unboxed as F
import qualified Control.Monad.Par.IO as Par
import qualified Control.Monad.Par as Par hiding (runParIO)


instance Show (NodeCounts a) where
  show = show . toList

node :: Edges entity anyEntity -> Node entity -> NodeCounts entity
node (Edges _ edgesPma) =
  let size = C.primMultiArrayOuterLength edgesPma
      in nodeWithSize size

nodeWithSize :: Int -> Node entity -> NodeCounts entity
nodeWithSize size (Node index) =
  NodeCounts (C.oneHotPrimArray size index 1)

nodeTargets :: Edges source target -> Node source -> NodeCounts target
nodeTargets (Edges targetAmount edgesPma) (Node sourceIndex) =
  let indexUnfold = fmap fromIntegral (A.primMultiArrayAt edgesPma sourceIndex)
      indexFold = E.indexCounts targetAmount
      countPa = B.fold indexFold indexUnfold
      in NodeCounts countPa

{-|
Count the occurrences of targets based on the occurrences of sources.

Utilizes concurrency.
-}
targets :: Edges source target -> NodeCounts source -> NodeCounts target
targets (Edges targetAmount edgesPma) (NodeCounts sourceCountsPa) =
  unsafePerformIO $ Par.runParIO $ do
    targetCountVarTable <- liftIO (D.newTVarArray 0 targetAmount)
    Par.parFor (Par.InclusiveRange 0 (pred (sizeofPrimArray sourceCountsPa))) $ \ sourceIndex ->
      case indexPrimArray sourceCountsPa sourceIndex of
        0 -> return ()
        sourceCount ->
          liftIO $
          B.forM_ (A.primMultiArrayAt edgesPma sourceIndex) $ \ targetIndex ->
          D.modifyTVarArrayAt targetCountVarTable (fromIntegral targetIndex) (+ sourceCount)
    targetCountsPa <- liftIO (D.freezeTVarArrayAsPrimArray targetCountVarTable)
    return (NodeCounts targetCountsPa)

toList :: NodeCounts entity -> [Word32]
toList (NodeCounts pa) = foldrPrimArray' (:) [] pa

toUnboxedVector :: NodeCounts entity -> F.Vector Word32
toUnboxedVector (NodeCounts pa) = C.primArrayUnboxedVector pa
