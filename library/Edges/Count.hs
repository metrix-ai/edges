module Edges.Count
(
  Count,
  run,
  targets,
)
where

import Edges.Prelude
import Edges.Internal.Types
import qualified PrimitiveExtras.UnfoldM as A
import qualified PrimitiveExtras.Pure as C
import qualified PrimitiveExtras.IO as D
import qualified DeferredFolds.UnfoldM as B


instance Semigroupoid Count where
  o (Count cAmount bToCIO) (Count bAmount aToBIO) =
    Count cAmount $ \ aIndex aWeight cCountVarTable -> do
      bCountVarTable <- D.newTVarArray 0 bAmount
      aToBIO aIndex aWeight bCountVarTable
      bCountPrimArray <- D.freezeTVarArrayAsPrimArray bCountVarTable
      block <- D.traversePrimArrayWithIndexConcurrently bCountPrimArray concurrency $ \ bIndex bCount -> do
        bToCIO bIndex bCount cCountVarTable
      block
    where
      concurrency = max (div numCapabilities 2) 1

run :: Count a b -> Node a -> NodeCounts b
run (Count bAmount aToBIO) (Node aIndex) =
  unsafePerformIO $ do
    bCountVarTable <- D.newTVarArray 0 bAmount
    aToBIO aIndex 1 bCountVarTable
    NodeCounts <$> D.freezeTVarArrayAsPrimArray bCountVarTable

targets :: Edges source target -> Count source target
targets (Edges targetArraySize edgesPma) =
  Count targetArraySize $ \ sourceIndex weight countVarTable ->
  case weight of
    0 -> return ()
    _ ->
      B.forM_ (A.primMultiArrayAt edgesPma sourceIndex) $ \ targetIndex ->
      D.modifyTVarArrayAt countVarTable (fromIntegral targetIndex) (+ weight)
