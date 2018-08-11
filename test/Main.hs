module Main where

import Prelude
import Test.QuickCheck
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Edges.Data
import qualified Edges.NodeCounting as NodeCounting
import qualified Edges.Data as Data
import qualified Edges.Gens as Gens
import qualified Data.Serialize as Cereal


main =
  defaultMain $
  testGroup "All tests" $
  [
    testGroup "Predefined bipartite" $ let
      edgeList :: [(Node (Proxy 1), Node (Proxy 2))]
      edgeList =
        fmap (bimap Node Node) $
        [
          (0, 0),
          (0, 1),
          (0, 2),
          (1, 0),
          (1, 1),
          (2, 0)
        ]
      (edges1, edges2) = Data.listBipartiteEdges edgeList
      in
        [
          testCase "Constructs the forward edges correctly" $ let
            reconstructedEdgeList = Data.edgesList edges1
            in assertEqual (show reconstructedEdgeList) edgeList reconstructedEdgeList
          ,
          testCase "Constructs the backward edges correctly" $ let
            reconstructedEdgeList = sort $ Data.edgesList edges2
            expectedEdgeList = sort $ fmap swap edgeList
            in assertEqual (show reconstructedEdgeList) expectedEdgeList reconstructedEdgeList
          ,
          testGroup "Counting at depth" $ let
            node = Node 1 :: Node (Proxy 1)
            in
              [
                testCase "1" $ let
                  nodeCountsList = NodeCounting.nodeTargets edges1 node & Data.nodeCountsList
                  in assertEqual (show nodeCountsList) [1, 1, 0] nodeCountsList
                ,
                testCase "2" $ let
                  nodeCountsList = NodeCounting.nodeTargets edges1 node & NodeCounting.targets edges2 & Data.nodeCountsList
                  in assertEqual (show nodeCountsList) [2, 2, 1] nodeCountsList
                ,
                testCase "3" $ let
                  nodeCountsList = NodeCounting.nodeTargets edges1 node & NodeCounting.targets edges2 & NodeCounting.targets edges1 & Data.nodeCountsList
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
    testProperty "Encoding/decoding with Cereal" $ forAll (Gens.bipartiteEdgesWithLimits 10 20) $ \ (edges1, edges2) ->
    Cereal.decode (Cereal.encode edges1) === Right edges1
  ]
