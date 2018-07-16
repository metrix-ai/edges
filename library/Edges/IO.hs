module Edges.IO
where

import Edges.Prelude
import Edges.Types
import Edges.Node ()
import Edges.NodeCounts ()
import Potoki.IO
import qualified Edges.Potoki.Produce as A
import qualified Potoki.Transform as B
import qualified Potoki.Cereal.Consume as C


encodeNodeCountsToFile :: FilePath -> Edges a x -> (Node a -> NodeCounts z) -> IO (Either IOException ())
encodeNodeCountsToFile file edges nodeCounts =
  produceAndConsume
    (A.nodeCounts edges nodeCounts)
    (C.encodeToFile file)
