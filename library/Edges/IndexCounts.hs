module Edges.IndexCounts
(
  IndexCounts,
  index,
  targets,
)
where

import Edges.Prelude hiding (index)
import Edges.Internal.Types
import qualified PrimitiveExtras.UnfoldM as A
import qualified PrimitiveExtras.Pure as C
import qualified PrimitiveExtras.IO as D
import qualified DeferredFolds.UnfoldM as B


index :: Edges entity anyEntity -> Index entity -> IndexCounts entity
index (Edges _ edgesPma) =
  let size = C.primMultiArrayOuterLength edgesPma
      in indexWithSize size

indexWithSize :: Int -> Index entity -> IndexCounts entity
indexWithSize size (Index index) =
  IndexCounts (C.oneHotPrimArray size index 1)

{-|
Count the occurrences of targets based on the occurrences of sources.

Utilizes concurrency.
-}
targets :: Edges source target -> IndexCounts source -> IndexCounts target
targets (Edges targetAmount edgesPma) (IndexCounts sourceCountsPa) =
  unsafePerformIO $ do
    targetCountVarTable <- D.newTVarArray 0 targetAmount
    waitTillDone <-
      D.traversePrimArrayWithIndexConcurrently sourceCountsPa concurrency $ \ sourceIndex sourceCount ->
      B.forM_ (A.primMultiArrayAt edgesPma sourceIndex) $ \ targetIndex ->
      D.modifyTVarArrayAt targetCountVarTable (fromIntegral targetIndex) (+ sourceCount)
    waitTillDone
    targetCountsPa <- D.freezeTVarArrayAsPrimArray targetCountVarTable
    return (IndexCounts targetCountsPa)
  where
    concurrency = max (div numCapabilities 2) 1
