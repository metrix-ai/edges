module Edges.NodeCounts
(
  NodeCounts,
  node,
  targets,
  toList,
)
where

import Edges.Prelude hiding (index, toList)
import Edges.Types
import Edges.Cereal.Instances ()
import qualified PrimitiveExtras.UnfoldM as A
import qualified PrimitiveExtras.Pure as C
import qualified PrimitiveExtras.IO as D
import qualified DeferredFolds.UnfoldM as B


instance Show (NodeCounts a) where
  show = show . toList

node :: Edges entity anyEntity -> Node entity -> NodeCounts entity
node (Edges _ edgesPma) =
  let size = C.primMultiArrayOuterLength edgesPma
      in nodeWithSize size

nodeWithSize :: Int -> Node entity -> NodeCounts entity
nodeWithSize size (Node index) =
  NodeCounts (C.oneHotPrimArray size index 1)

{-|
Count the occurrences of targets based on the occurrences of sources.

Utilizes concurrency.
-}
targets :: Edges source target -> NodeCounts source -> NodeCounts target
targets (Edges targetAmount edgesPma) (NodeCounts sourceCountsPa) =
  unsafePerformIO $ do
    targetCountVarTable <- D.newTVarArray 0 targetAmount
    waitTillDone <-
      D.traversePrimArrayWithIndexConcurrently sourceCountsPa concurrency $ \ sourceIndex sourceCount ->
      case sourceCount of
        0 -> return ()
        _ ->
          B.forM_ (A.primMultiArrayAt edgesPma sourceIndex) $ \ targetIndex ->
          D.modifyTVarArrayAt targetCountVarTable (fromIntegral targetIndex) (+ sourceCount)
    waitTillDone
    targetCountsPa <- D.freezeTVarArrayAsPrimArray targetCountVarTable
    return (NodeCounts targetCountsPa)
  where
    concurrency = numCapabilities * 2

toList :: NodeCounts entity -> [Word32]
toList (NodeCounts pa) = foldrPrimArray' (:) [] pa
