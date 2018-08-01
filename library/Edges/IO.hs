module Edges.IO
where

import Edges.Prelude
import Edges.Data
import Potoki.IO
import qualified Edges.Potoki.Produces as A
import qualified Potoki.Cereal.Consume as C


encodeNodeCountsToFile :: FilePath -> Amount a -> (Node a -> NodeCounts z) -> IO (Either IOException ())
encodeNodeCountsToFile file amount nodeCounts =
  produceAndConsume
    (A.nodeCounts amount nodeCounts)
    (C.encodeToFile file)
