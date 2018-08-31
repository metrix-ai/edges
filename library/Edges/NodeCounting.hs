module Edges.NodeCounting
(
  NodeCounts,
  Node,
  Amount,
  Edges,
  nodeTargets,
  targets,
)
where

import Edges.Prelude hiding (index, toList)
import Edges.Types
import Edges.Functions
import Edges.Instances ()
import qualified PrimitiveExtras.PrimArray as PrimArray
import qualified PrimitiveExtras.PrimMultiArray as PrimMultiArray
import qualified PrimitiveExtras.TVarArray as TVarArray
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import qualified Data.Vector.Unboxed as UnboxedVector
import qualified Data.Vector.Unboxed.Mutable as MutableUnboxedVector


nodeTargets :: Edges source target -> Node source -> NodeCounts target
nodeTargets (Edges targetAmount edgesPma) (Node sourceIndex) = do
  unsafePerformIO $ do
    targetCountsMVec <- MutableUnboxedVector.new targetAmount
    UnfoldlM.forM_ (fmap fromIntegral $ PrimMultiArray.toUnfoldlAtM edgesPma sourceIndex) $ \ targetIndex -> do
      targetCount <- MutableUnboxedVector.read targetCountsMVec targetIndex
      MutableUnboxedVector.unsafeWrite targetCountsMVec targetIndex (targetCount + 1)
    targetCountsVec <- UnboxedVector.unsafeFreeze targetCountsMVec
    return (NodeCounts targetCountsVec)

{-|
Count the occurrences of targets based on the occurrences of sources.
-}
targets :: Edges source target -> NodeCounts source -> NodeCounts target
targets = targetsWithMinSourceAmount 1

{-|
Count the occurrences of targets based on the occurrences of sources.

This function can be used to reduce the amount of computation by excluding the nodes,
which don't make much difference.
-}
targetsWithMinSourceAmount :: Word128 -> Edges source target -> NodeCounts source -> NodeCounts target
targetsWithMinSourceAmount minSourceCount (Edges targetAmount edgesPma) (NodeCounts sourceCountsVec) =
  unsafePerformIO $ do
    targetCountsMVec <- MutableUnboxedVector.new targetAmount
    flip UnboxedVector.imapM_ sourceCountsVec $ \ sourceIndex sourceCount -> if sourceCount >= minSourceCount
      then UnfoldlM.forM_ (fmap fromIntegral $ PrimMultiArray.toUnfoldlAtM edgesPma sourceIndex) $ \ targetIndex -> do
        targetCount <- MutableUnboxedVector.read targetCountsMVec targetIndex
        MutableUnboxedVector.unsafeWrite targetCountsMVec targetIndex (targetCount + sourceCount)
      else return ()
    targetCountsVec <- UnboxedVector.unsafeFreeze targetCountsMVec
    return (NodeCounts targetCountsVec)
