module Main where

import Prelude
import Test.QuickCheck
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Edges.Edges as A
import qualified Edges.NodeCounts as B
import qualified Edges.Node as C
import qualified Data.Serialize as D


main =
  defaultMain $
  testGroup "All tests" $
  [
    testGroup "Predefined bipartite" $ let
      edgeList :: [(C.Node (Proxy 1), C.Node (Proxy 2))]
      edgeList =
        fmap (bimap C.Node C.Node) $
        [
          (0, 0),
          (0, 1),
          (0, 2),
          (1, 0),
          (1, 1),
          (2, 0)
        ]
      (edges1, edges2) = A.listBipartite edgeList
      in
        [
          testCase "Constructs the forward edges correctly" $ let
            reconstructedEdgeList = A.toAssocList edges1
            in assertEqual (show reconstructedEdgeList) edgeList reconstructedEdgeList
          ,
          testCase "Constructs the backward edges correctly" $ let
            reconstructedEdgeList = sort $ A.toAssocList edges2
            expectedEdgeList = sort $ fmap swap edgeList
            in assertEqual (show reconstructedEdgeList) expectedEdgeList reconstructedEdgeList
        ]
    ,
    testProperty "Encoding/decoding with Cereal" $ forAll (A.genBipartiteWithLimits 10 20) $ \ (edges1, edges2) ->
    D.decode (D.encode edges1) === Right edges1
  ]
