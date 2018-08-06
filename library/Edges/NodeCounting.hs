module Edges.NodeCounting
(
  NodeCounts,
  Node,
  Amount,
  Edges,
  node,
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
import qualified DeferredFolds.UnfoldM as UnfoldM
import qualified Data.Vector.Unboxed as UnboxedVector
import qualified Control.Monad.Par.IO as Par
import qualified Control.Monad.Par as Par hiding (runParIO)


node :: Edges source target -> Node source -> NodeCounts source
node edges = 
  nodeWithAmount (edgesSourceAmount edges)

nodeWithAmount :: Amount entity -> Node entity -> NodeCounts entity
nodeWithAmount (Amount size) (Node index) =
  NodeCounts (PrimArray.oneHot size index 1)

nodeTargets :: Edges source target -> Node source -> NodeCounts target
nodeTargets (Edges targetAmount edgesPma) (Node sourceIndex) =
  let indexUnfold = fmap fromIntegral (PrimMultiArray.toUnfoldAtM edgesPma sourceIndex)
      indexFold = PrimArray.indexCountsFold targetAmount
      countPa = UnfoldM.fold indexFold indexUnfold
      in NodeCounts countPa

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
targetsWithMinSourceAmount :: Word32 -> Edges source target -> NodeCounts source -> NodeCounts target
targetsWithMinSourceAmount minSourceCount (Edges targetAmount edgesPma) (NodeCounts sourceCountsPa) =
  unsafePerformIO $ do
    targetCountsMpa <- newPrimArray targetAmount
    forMInAscendingRange_ 0 (sizeofPrimArray sourceCountsPa) $ \ sourceIndex -> let
      sourceCount = indexPrimArray sourceCountsPa sourceIndex
      in if sourceCount >= minSourceCount
        then UnfoldM.forM_ (fmap fromIntegral $ PrimMultiArray.toUnfoldAtM edgesPma sourceIndex) $ \ targetIndex -> do
          targetCount <- readPrimArray targetCountsMpa targetIndex
          writePrimArray targetCountsMpa targetIndex (targetCount + sourceCount)
        else return ()
    targetCountsPa <- unsafeFreezePrimArray targetCountsMpa
    return (NodeCounts targetCountsPa)

{-|
Count the occurrences of targets based on the occurrences of sources.

Utilizes parallellism.
-}
targetsParallelly :: Edges source target -> NodeCounts source -> NodeCounts target
targetsParallelly (Edges targetAmount edgesPma) (NodeCounts sourceCountsPa) =
  unsafePerformIO $ Par.runParIO $ do
    targetCountVarTable <- liftIO (TVarArray.new 0 targetAmount)
    Par.parFor (Par.InclusiveRange 0 (pred (sizeofPrimArray sourceCountsPa))) $ \ sourceIndex ->
      case indexPrimArray sourceCountsPa sourceIndex of
        0 -> return ()
        sourceCount ->
          liftIO $
          UnfoldM.forM_ (PrimMultiArray.toUnfoldAtM edgesPma sourceIndex) $ \ targetIndex ->
          TVarArray.modifyAt targetCountVarTable (fromIntegral targetIndex) (+ sourceCount)
    targetCountsPa <- liftIO (TVarArray.freezeAsPrimArray targetCountVarTable)
    return (NodeCounts targetCountsPa)
