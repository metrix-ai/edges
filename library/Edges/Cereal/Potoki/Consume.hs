module Edges.Cereal.Potoki.Consume where

import Edges.Prelude
import Edges.Types

import Potoki.Core.Consume (Consume)
import Potoki.Cereal.Consume (encodeToFile)

serializeEdges :: FilePath -> Consume (Edge from to) (Either IOException ())
serializeEdges = encodeToFile
