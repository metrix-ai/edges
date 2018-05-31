module Edges.Cereal.Potoki.Produce where

import Edges.Prelude
import Edges.Types

import Potoki.Core.Produce (Produce)
import Potoki.Cereal.Produce (fileDecoded)

deserializeEdges :: FilePath -> Produce (Either IOException (Either Text (Edge from to)))
deserializeEdges = fileDecoded
