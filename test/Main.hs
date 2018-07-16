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
          ,
          testGroup "Counting at depth" $ let
            node = C.Node 1 :: C.Node (Proxy 1)
            in
              [
                testCase "0" $ let
                  nodeCountsList = B.node edges1 node & B.toList
                  in assertEqual (show nodeCountsList) [0, 1, 0] nodeCountsList
                ,
                testCase "1, unoptimized" $ let
                  nodeCountsList = B.node edges1 node & B.targets edges1 & B.toList
                  in assertEqual (show nodeCountsList) [1, 1, 0] nodeCountsList
                ,
                testCase "1" $ let
                  nodeCountsList = B.nodeTargets edges1 node & B.toList
                  in assertEqual (show nodeCountsList) [1, 1, 0] nodeCountsList
                ,
                testCase "2" $ let
                  nodeCountsList = B.nodeTargets edges1 node & B.targets edges2 & B.toList
                  in assertEqual (show nodeCountsList) [2, 2, 1] nodeCountsList
                ,
                testCase "3" $ let
                  nodeCountsList = B.nodeTargets edges1 node & B.targets edges2 & B.targets edges1 & B.toList
                  in
                    {-
                    [0, 1]
                    [[0, 1, 2], [0, 1]]
                    [[[0, 1, 2], [0, 1], [0]], [[0, 1, 2], [0, 1]]]
                    [5, 4, 2]
                    -}
                    assertEqual (show nodeCountsList) [5, 4, 2] nodeCountsList
              ]
        ]
    ,
    testProperty "Encoding/decoding with Cereal" $ forAll (A.genBipartiteWithLimits 10 20) $ \ (edges1, edges2) ->
    D.decode (D.encode edges1) === Right edges1
  ]
